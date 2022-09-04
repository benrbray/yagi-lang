import * as path from 'path';

import {
	CancellationToken,
	ExtensionContext,
	OutputChannel,
	Hover,
	Position,
	ProviderResult,
	TextDocument,
	Uri,
	window,
	workspace,
	WorkspaceFolder
} from 'vscode';

import {
	Executable,
	ExecutableOptions,
	HoverRequest,
	LanguageClient,
	LanguageClientOptions,
	Logger,
	ProvideHoverSignature,
	RevealOutputChannelOn,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';
import { LanguageServerNotFoundError } from './error';
import { rangeHover } from './rangeHover';
import { findLanguageServer } from './find-language-server';
import { ExtensionLogger, LogLevel } from './logger';
import { expandPathVariables } from './utils';

import { getYagiExtensionConfig, YagiExtensionConfig } from './yagi-config';

////////////////////////////////////////////////////////////

// current map of documents & folders to language servers
// `null` value indicates that server is currently starting up
const langClients: Map<string, LanguageClient|null> = new Map();

var globalOutput: OutputChannel|null = null;
var globalLogger: Logger|null = null;

////////////////////////////////////////////////////////////

export async function activate(context: ExtensionContext): Promise<void> {
	// create global log
	globalOutput = window.createOutputChannel("Yagi (Global)");
	globalLogger = new ExtensionLogger("yagi-global", LogLevel.Debug, globalOutput, null);

	// (Possibly) launch the language server every time a document is
	// opened, so that it works across multiple workspace folders
	// TODO (Ben @ 2022/08/14) support LSP workspaceFolders capability
	workspace.onDidOpenTextDocument(async (document: TextDocument) => await activeServer(context, document));
	
	ensureServerForAllOpenDocuments(context);

	// Stop the server from any workspace folders that are removed.
	workspace.onDidChangeWorkspaceFolders((event) => {
		globalLogger?.log("workspace configuration changed");

		for (const folder of event.removed) {
			const client = langClients.get(folder.uri.toString());
			if (client) {
				const uri = folder.uri.toString();
				stopAndDeleteClient(client, uri, "workspace folder closed");
			}
		}
	});

	workspace.onDidChangeConfiguration((event) => {
		// shutdown and recreate existing clients/servers if necessary
		const keys = langClients.keys();
		for(const clientKey of keys) {
			// TODO (Ben @ 2022/08/14) what to do if config changes while client is starting up?
			const client = langClients.get(clientKey);
			const clientUri = Uri.parse(clientKey);
			if(!client) { continue; }

			// only handle config changes related to this extension
			if(!event.affectsConfiguration("yagi", clientUri)) { continue; }
	
			// delete the client and create a new client from scratch
			stopAndDeleteClient(client, clientKey, "client configuration change");
			activateServerForFolder(context, clientUri);
		}

		// make sure that every open yagi file has an associated language server,
		// since some may have previously failed to start due to configuration errors
		ensureServerForAllOpenDocuments(context);
	});
}

function ensureServerForAllOpenDocuments(context: ExtensionContext) {
	workspace.textDocuments.forEach(async (document: TextDocument) => await activeServer(context, document));
}

////////////////////////////////////////////////////////////

function stopAndDeleteClient(client: LanguageClient, clientUri: string, reason: string) {
	globalLogger?.info(`deleting client for folder: ${clientUri}`);
	langClients.delete(clientUri);
	globalLogger?.info(`stopping language server (${reason})`);
	client.outputChannel.hide();
	client.outputChannel.dispose();
	client.stop();
}

////////////////////////////////////////////////////////////

export async function activeServer(context: ExtensionContext, document: TextDocument): Promise<void> {
	// we only care about *.yagi files
	if( (document.languageId !== 'yagi') ||
	    (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')
	){ return; }

	activateServerForFolder(
		context,
		document.uri
	);
}

export async function activateServerForFolder(context: ExtensionContext, uri: Uri): Promise<void> {
	// get workspace folder
	const folder = workspace.getWorkspaceFolder(uri);
	if(!folder) {
		window.showErrorMessage(`No workspace folder found for ${uri}.  Standalone files are currently unsupported.`);
		return;
	}

	// set unique name per workspace folder, in order to maintain one server per workspace
	// TODO (Ben @ 2022/08/14) handle standalone files
	const clientKey: string         = folder ? folder.uri.toString() : uri.toString();
	if(langClients.has(clientKey)) { return; }
	
	const currentWorkingDir: string = folder ? folder.uri.fsPath     : path.dirname(uri.fsPath);
	const langName                  = "Yagi" + (folder ? ` (${folder.name})` : "");

	// get extension settings for this workspace
	globalLogger?.info(`starting client for folder ${uri}`)
	const yagiConfig: YagiExtensionConfig = getYagiExtensionConfig(uri); 

	// configure output / logging
	const outputChannel = globalOutput || window.createOutputChannel(langName);
	const logFilePath: string|null =
		yagiConfig.logFile !== ""
		? path.resolve(currentWorkingDir, expandPathVariables(yagiConfig.logFile))
		: null;

	const logger: Logger = new ExtensionLogger("yagi-client", yagiConfig.logLevel.client, outputChannel, logFilePath)
	if(logFilePath) { logger.info(`writing language client log to file ${logFilePath}`); }
	else            { logger.info(`no log file specified ; to save logs to a file, set \`yagi.logFile\` in extension settings`); }

	// check for language server executable
	let serverExecutablePath: string;
	try {
		serverExecutablePath = await findLanguageServer(logger, yagiConfig, folder);
	} catch(e) {
		// handle specific errors
		if(e instanceof LanguageServerNotFoundError) {
			window.showErrorMessage(e.message);
		}

		// general stack-trace printing
		if (e instanceof Error && e.stack) {
			logger?.error(`${e.stack}`);
    }

		// cancel server startup
		logger.info(`cancelling client startup for folder ${uri} due to error`);
		return;
	}

	// workspace vs standalone file
	const serverCwd: string|undefined = folder ? undefined : path.dirname(uri.fsPath)

	// locate server executable
	makeLanguageClient({
		langName: langName,
		clientKey: clientKey,
		serverOptions: makeServerOptions({
			serverExecutablePath: serverExecutablePath,
			serverExtraArgs: yagiConfig.serverExtraArgs,
			cwd: serverCwd
		}),
		clientOptions: makeClientOptions({
			folder: folder,
			langName: langName,
			outputChannel: outputChannel,
		})
	})
}

export function makeLanguageClient(
	args: {
		langName: string,
		clientKey: string,
		serverOptions: ServerOptions,
		clientOptions: LanguageClientOptions
	}
): void {
	// set key to null while server is starting up
	langClients.set(args.clientKey, null);

	// provideHover middleware
	// (adapted from [rust-analyzer](https://github.com/rust-lang/rust-analyzer/pull/9693/files#))
	async function provideHover(
		document: TextDocument,
		position: Position,
		token: CancellationToken,
		_next: ProvideHoverSignature
	): Promise<Hover|null|undefined> {
		const editor = window.activeTextEditor;
		const range
			= editor?.selection?.contains(position) 
			? langClient.code2ProtocolConverter.asRange(editor.selection)
			: undefined;

		try {
			return langClient.protocol2CodeConverter.asHover(
				await langClient.sendRequest(
					rangeHover,
					{
						textDocument: langClient.code2ProtocolConverter.asTextDocumentIdentifier(document),
						position: langClient.code2ProtocolConverter.asPosition(position),
						...(range && { range: range })
					},
					token
				)
			);
		} catch(error: unknown) {
			langClient.handleFailedRequest(
					HoverRequest.type,
					error,
					null
			);

			return null;
		}
	}

	// create and start language client
	const langClient: LanguageClient =
		new LanguageClient(
			"haskell-lsp",
			args.langName,
			args.serverOptions,
			{
				...args.clientOptions,
				middleware: {
					provideHover
				}
			}
		);

	langClient.registerProposedFeatures();
	langClient.start();
	langClients.set(args.clientKey, langClient);
}

////////////////////////////////////////////////////////////

function makeServerOptions(
	args: {
		serverExecutablePath: string,
		serverExtraArgs: string,
		cwd: string|undefined
	}): ServerOptions {
	// language server command line args
	let serverArgs: string[] = ["--lsp"]
  const serverExtraArgs: string = args.serverExtraArgs;
  if (serverExtraArgs !== '') {
    serverArgs = serverArgs.concat(serverExtraArgs.split(' '));
  }

	// language server executable options
	const serverEnvironment: { [key:string]: string } = { }
	const exeOptions: ExecutableOptions = {
		cwd: args.cwd,
		env: { ...process.env, ...serverEnvironment }
	}

	// no difference between run/debug configuration
	const run: Executable = {
		command: args.serverExecutablePath,
		args: serverArgs,
		options: exeOptions
	};

	const debug: Executable = {
		command: args.serverExecutablePath,
		args: serverArgs,
		options: exeOptions
	};

	return { run, debug };
}

function makeClientOptions(
	args: {
		folder?: WorkspaceFolder,
		outputChannel: OutputChannel,
		langName: string
	}
): LanguageClientOptions {
	const pat = args.folder ? `${args.folder.uri.fsPath}/**/*` : '**/*';
	return {
		documentSelector: [ { scheme: "file", language: "yagi" } ],
		synchronize: { configurationSection: "yagi" },
		diagnosticCollectionName: args.langName,
		revealOutputChannelOn: RevealOutputChannelOn.Never,
		outputChannel: args.outputChannel,
		outputChannelName: args.langName,
		workspaceFolder: args.folder,
		middleware: {
			async provideHover(document, position, token, _next): Promise<Hover> {
				

				return (this as any);
			}
		}
	};
}

////////////////////////////////////////////////////////////

async function restartAllClients() {
	for(const client of langClients.values()) {
		if(!client) { return; }
		client.info("stopping client");
		await client.stop();
		client.info("starting client");
		client.start();
	};
}

////////////////////////////////////////////////////////////

export async function deactivate(): Promise<void> {
	// deactivate all language servers
  const promises: Promise<void>[] = [];
  for (const client of langClients.values()) {
    if (client) {
      promises.push(client.stop());
    }
  }
  await Promise.all(promises);
}