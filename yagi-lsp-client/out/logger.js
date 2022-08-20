"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.ExtensionLogger = exports.LogLevel = void 0;
const fs = require("fs");
////////////////////////////////////////////////////////////
var LogLevel;
(function (LogLevel) {
    LogLevel[LogLevel["Off"] = 0] = "Off";
    LogLevel[LogLevel["Error"] = 1] = "Error";
    LogLevel[LogLevel["Warn"] = 2] = "Warn";
    LogLevel[LogLevel["Info"] = 3] = "Info";
    LogLevel[LogLevel["Debug"] = 4] = "Debug";
})(LogLevel = exports.LogLevel || (exports.LogLevel = {}));
function formatTimestamp(d) {
    return d.toISOString().replace('T', ' ').replace('Z', '0000');
}
////////////////////////////////////////////////////////////
class ExtensionLogger {
    constructor(name, level, channel, logFile, copyLogsTo) {
        this.name = name;
        this.level = level;
        this.channel = channel;
        this.logFile = logFile;
        this.copyLogsTo = copyLogsTo;
    }
    /* ---- Logger implementation ------------------------- */
    warn(message) { this.logLevel(LogLevel.Warn, message); }
    info(message) { this.logLevel(LogLevel.Info, message); }
    error(message) { this.logLevel(LogLevel.Error, message); }
    log(message) { this.logLevel(LogLevel.Debug, message); }
    /* ---------------------------------------------------- */
    logLevel(level, msg) {
        if (level <= this.level) {
            this.write(`[${this.name}] ${LogLevel[level].toUpperCase()} ${msg}`);
        }
    }
    write(msg) {
        const timedMessage = `[${formatTimestamp(new Date())}] ${msg}`;
        this.channel.appendLine(timedMessage);
        if (this.logFile) {
            fs.appendFileSync(this.logFile, timedMessage + "\n");
        }
    }
}
exports.ExtensionLogger = ExtensionLogger;
//# sourceMappingURL=logger.js.map