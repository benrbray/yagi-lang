export class LanguageServerNotFoundError extends Error {
	constructor(
		public readonly path: string
	) {
		super(`Could not find a language server binary at ${path}!  Set a new \`serverExecutablePath\` in the extension settings.`);
	}
}