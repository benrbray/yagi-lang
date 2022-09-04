"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.makeLanguageClient = exports.activateServerForFolder = exports.activeServer = exports.activate = void 0;
const path = require("path");
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
const error_1 = require("./error");
const rangeHover_1 = require("./rangeHover");
const find_language_server_1 = require("./find-language-server");
const logger_1 = require("./logger");
const utils_1 = require("./utils");
const yagi_config_1 = require("./yagi-config");
////////////////////////////////////////////////////////////
// current map of documents & folders to language servers
// `null` value indicates that server is currently starting up
const langClients = new Map();
var globalOutput = null;
var globalLogger = null;
////////////////////////////////////////////////////////////
async function activate(context) {
    // create global log
    globalOutput = vscode_1.window.createOutputChannel("Yagi (Global)");
    globalLogger = new logger_1.ExtensionLogger("yagi-global", logger_1.LogLevel.Debug, globalOutput, null);
    // (Possibly) launch the language server every time a document is
    // opened, so that it works across multiple workspace folders
    // TODO (Ben @ 2022/08/14) support LSP workspaceFolders capability
    vscode_1.workspace.onDidOpenTextDocument(async (document) => await activeServer(context, document));
    ensureServerForAllOpenDocuments(context);
    // Stop the server from any workspace folders that are removed.
    vscode_1.workspace.onDidChangeWorkspaceFolders((event) => {
        globalLogger?.log("workspace configuration changed");
        for (const folder of event.removed) {
            const client = langClients.get(folder.uri.toString());
            if (client) {
                const uri = folder.uri.toString();
                stopAndDeleteClient(client, uri, "workspace folder closed");
            }
        }
    });
    vscode_1.workspace.onDidChangeConfiguration((event) => {
        // shutdown and recreate existing clients/servers if necessary
        const keys = langClients.keys();
        for (const clientKey of keys) {
            // TODO (Ben @ 2022/08/14) what to do if config changes while client is starting up?
            const client = langClients.get(clientKey);
            const clientUri = vscode_1.Uri.parse(clientKey);
            if (!client) {
                continue;
            }
            // only handle config changes related to this extension
            if (!event.affectsConfiguration("yagi", clientUri)) {
                continue;
            }
            // delete the client and create a new client from scratch
            stopAndDeleteClient(client, clientKey, "client configuration change");
            activateServerForFolder(context, clientUri);
        }
        // make sure that every open yagi file has an associated language server,
        // since some may have previously failed to start due to configuration errors
        ensureServerForAllOpenDocuments(context);
    });
}
exports.activate = activate;
function ensureServerForAllOpenDocuments(context) {
    vscode_1.workspace.textDocuments.forEach(async (document) => await activeServer(context, document));
}
////////////////////////////////////////////////////////////
function stopAndDeleteClient(client, clientUri, reason) {
    globalLogger?.info(`deleting client for folder: ${clientUri}`);
    langClients.delete(clientUri);
    globalLogger?.info(`stopping language server (${reason})`);
    client.outputChannel.hide();
    client.outputChannel.dispose();
    client.stop();
}
////////////////////////////////////////////////////////////
async function activeServer(context, document) {
    // we only care about *.yagi files
    if ((document.languageId !== 'yagi') ||
        (document.uri.scheme !== 'file' && document.uri.scheme !== 'untitled')) {
        return;
    }
    activateServerForFolder(context, document.uri);
}
exports.activeServer = activeServer;
async function activateServerForFolder(context, uri) {
    // get workspace folder
    const folder = vscode_1.workspace.getWorkspaceFolder(uri);
    if (!folder) {
        vscode_1.window.showErrorMessage(`No workspace folder found for ${uri}.  Standalone files are currently unsupported.`);
        return;
    }
    // set unique name per workspace folder, in order to maintain one server per workspace
    // TODO (Ben @ 2022/08/14) handle standalone files
    const clientKey = folder ? folder.uri.toString() : uri.toString();
    if (langClients.has(clientKey)) {
        return;
    }
    const currentWorkingDir = folder ? folder.uri.fsPath : path.dirname(uri.fsPath);
    const langName = "Yagi" + (folder ? ` (${folder.name})` : "");
    // get extension settings for this workspace
    globalLogger?.info(`starting client for folder ${uri}`);
    const yagiConfig = yagi_config_1.getYagiExtensionConfig(uri);
    // configure output / logging
    const outputChannel = globalOutput || vscode_1.window.createOutputChannel(langName);
    const logFilePath = yagiConfig.logFile !== ""
        ? path.resolve(currentWorkingDir, utils_1.expandPathVariables(yagiConfig.logFile))
        : null;
    const logger = new logger_1.ExtensionLogger("yagi-client", yagiConfig.logLevel.client, outputChannel, logFilePath);
    if (logFilePath) {
        logger.info(`writing language client log to file ${logFilePath}`);
    }
    else {
        logger.info(`no log file specified ; to save logs to a file, set \`yagi.logFile\` in extension settings`);
    }
    // check for language server executable
    let serverExecutablePath;
    try {
        serverExecutablePath = await find_language_server_1.findLanguageServer(logger, yagiConfig, folder);
    }
    catch (e) {
        // handle specific errors
        if (e instanceof error_1.LanguageServerNotFoundError) {
            vscode_1.window.showErrorMessage(e.message);
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
    const serverCwd = folder ? undefined : path.dirname(uri.fsPath);
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
    });
}
exports.activateServerForFolder = activateServerForFolder;
function makeLanguageClient(args) {
    // set key to null while server is starting up
    langClients.set(args.clientKey, null);
    // provideHover middleware
    // (adapted from [rust-analyzer](https://github.com/rust-lang/rust-analyzer/pull/9693/files#))
    async function provideHover(document, position, token, _next) {
        const editor = vscode_1.window.activeTextEditor;
        const range = editor?.selection?.contains(position)
            ? langClient.code2ProtocolConverter.asRange(editor.selection)
            : undefined;
        try {
            return langClient.protocol2CodeConverter.asHover(await langClient.sendRequest(rangeHover_1.rangeHover, {
                textDocument: langClient.code2ProtocolConverter.asTextDocumentIdentifier(document),
                position: langClient.code2ProtocolConverter.asPosition(position),
                ...(range && { range: range })
            }, token));
        }
        catch (error) {
            langClient.handleFailedRequest(node_1.HoverRequest.type, error, null);
            return null;
        }
    }
    // create and start language client
    const langClient = new node_1.LanguageClient("haskell-lsp", args.langName, args.serverOptions, {
        ...args.clientOptions,
        middleware: {
            provideHover
        }
    });
    langClient.registerProposedFeatures();
    langClient.start();
    langClients.set(args.clientKey, langClient);
}
exports.makeLanguageClient = makeLanguageClient;
////////////////////////////////////////////////////////////
function makeServerOptions(args) {
    // language server command line args
    let serverArgs = ["--lsp"];
    const serverExtraArgs = args.serverExtraArgs;
    if (serverExtraArgs !== '') {
        serverArgs = serverArgs.concat(serverExtraArgs.split(' '));
    }
    // language server executable options
    const serverEnvironment = {};
    const exeOptions = {
        cwd: args.cwd,
        env: { ...process.env, ...serverEnvironment }
    };
    // no difference between run/debug configuration
    const run = {
        command: args.serverExecutablePath,
        args: serverArgs,
        options: exeOptions
    };
    const debug = {
        command: args.serverExecutablePath,
        args: serverArgs,
        options: exeOptions
    };
    return { run, debug };
}
function makeClientOptions(args) {
    const pat = args.folder ? `${args.folder.uri.fsPath}/**/*` : '**/*';
    return {
        documentSelector: [{ scheme: "file", language: "yagi" }],
        synchronize: { configurationSection: "yagi" },
        diagnosticCollectionName: args.langName,
        revealOutputChannelOn: node_1.RevealOutputChannelOn.Never,
        outputChannel: args.outputChannel,
        outputChannelName: args.langName,
        workspaceFolder: args.folder,
        middleware: {
            async provideHover(document, position, token, _next) {
                return this;
            }
        }
    };
}
////////////////////////////////////////////////////////////
async function restartAllClients() {
    for (const client of langClients.values()) {
        if (!client) {
            return;
        }
        client.info("stopping client");
        await client.stop();
        client.info("starting client");
        client.start();
    }
    ;
}
////////////////////////////////////////////////////////////
async function deactivate() {
    // deactivate all language servers
    const promises = [];
    for (const client of langClients.values()) {
        if (client) {
            promises.push(client.stop());
        }
    }
    await Promise.all(promises);
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map