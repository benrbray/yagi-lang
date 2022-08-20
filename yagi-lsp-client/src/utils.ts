
import * as os from 'os';

import { WorkspaceFolder } from 'vscode';

////////////////////////////////////////////////////////////

export type EnvVars = { [key:string]: string };

export function expandPathVariables(path: string, folder?: WorkspaceFolder) {
  path = path
		.replace('${HOME}', os.homedir)
		.replace('${home}', os.homedir)
		.replace(/^~/, os.homedir);

	if(!folder) { return path; }

	path = path
		.replace('${workspaceRoot}', folder.uri.path);

  return path;
}