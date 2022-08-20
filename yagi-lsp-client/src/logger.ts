import { OutputChannel } from 'vscode';
import { Logger } from 'vscode-languageclient';
import * as fs from 'fs';

////////////////////////////////////////////////////////////

export enum LogLevel {
  Off,
  Error,
  Warn,
  Info,
  Debug
}

function formatTimestamp(d: Date): string {
	return d.toISOString().replace('T', ' ').replace('Z', '0000')
}

////////////////////////////////////////////////////////////

export class ExtensionLogger implements Logger {

	constructor(
		public readonly name: string,
    public readonly level: LogLevel,
		public readonly channel: OutputChannel,
		public readonly logFile: string | null,
		public readonly copyLogsTo?: Logger
	) { }

	/* ---- Logger implementation ------------------------- */

	warn(message: string):  void { this.logLevel(LogLevel.Warn,  message); }
	info(message: string):  void { this.logLevel(LogLevel.Info,  message); }
	error(message: string): void { this.logLevel(LogLevel.Error, message); }
	log(message: string):   void { this.logLevel(LogLevel.Debug, message); }

	/* ---------------------------------------------------- */

	private logLevel(level: LogLevel, msg: string): void {
		if(level <= this.level) {
			this.write(`[${this.name}] ${LogLevel[level].toUpperCase()} ${msg}`);
		}
	}

	private write(msg: string): void {
		const timedMessage = `[${formatTimestamp(new Date())}] ${msg}`;
		this.channel.appendLine(timedMessage);
		if(this.logFile) {
			fs.appendFileSync(this.logFile, timedMessage + "\n");
		}
	}

}