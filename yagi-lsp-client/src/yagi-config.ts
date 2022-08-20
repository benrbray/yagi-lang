import {
	Uri,
	workspace
} from 'vscode';

import { LogLevel } from './logger';


////////////////////////////////////////////////////////////

export type YagiTraceServer = "off" | "messages" | "verbose";

export interface YagiExtensionConfig {
	logLevel: {
		server: YagiTraceServer,
		client: LogLevel
	},
	logFile: string,
	serverExecutablePath: string,
	serverExtraArgs: string
}

////////////////////////////////////////////////////////////

export function getYagiExtensionConfig(uri: Uri): YagiExtensionConfig {
	const config = workspace.getConfiguration("yagi", uri);

	return {
		logLevel: {
			server: config.get<YagiTraceServer>("logLevel.server", "off"),
			client: clientLogLevel(config.get<string>("logLevel.server", "off"))
		},
		logFile:              config.get<string>("logFile", ""),
		serverExecutablePath: config.get<string>("serverExecutablePath", ""),
		serverExtraArgs:      config.get<string>("serverExtraArgs", "")
	};
}

////////////////////////////////////////////////////////////

function clientLogLevel(level: string): LogLevel {
	switch(level) {
		case "off":   return LogLevel.Off;
		case "error": return LogLevel.Error;
		case "warn":  return LogLevel.Warn;
		case "info":  return LogLevel.Info;
		case "debug": return LogLevel.Debug;
		default:      return LogLevel.Info;
	}
}