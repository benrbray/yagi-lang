import { WorkspaceFolder } from "vscode";
import { Logger } from "vscode-languageclient";
import * as child_process from 'child_process';
import * as which from 'which';

import { EnvVars, expandPathVariables } from "./utils";
import { YagiExtensionConfig } from "./yagi-config";
import { LanguageServerNotFoundError } from "./error";

////////////////////////////////////////////////////////////////////////////////

export async function findLanguageServer(
	logger: Logger,
	yagiConfig: YagiExtensionConfig,
	folder?: WorkspaceFolder
): Promise<string> {
  logger.info(`attempting to find the server executable in: ${yagiConfig.serverExecutablePath}`);
	const expandedPath = expandPathVariables(yagiConfig.serverExecutablePath, folder);
  logger.log(`location after path variables substitution: ${expandedPath}`);

  if (executableExists(expandedPath)) {
    return expandedPath;
  } else {
    throw new LanguageServerNotFoundError(expandedPath);
  }
}

export function executableExists(exe: string): boolean {
  const isWindows = process.platform === 'win32';
  
	let newEnv: EnvVars = { };
  newEnv = { ...(process.env as EnvVars), ...newEnv };
  
	const cmd: string = isWindows ? 'where' : 'which';
  const out = child_process.spawnSync(cmd, [exe], { env: newEnv });
  return out.status === 0 || (which.sync(exe, { nothrow: true, path: newEnv.PATH }) ?? '') !== '';
}