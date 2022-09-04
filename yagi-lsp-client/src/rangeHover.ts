import {
	Position,
	Hover,
	Range,
	RequestType,
	TextDocumentIdentifier,
	WorkDoneProgressParams
} from 'vscode-languageclient';

// adapted from
// - [rust-analyzer](https://github.com/rust-lang/rust-analyzer/pull/9693/files#)
// - [metals-vscode](https://github.com/scalameta/metals-vscode/blob/66c2bcc36fbbeb9a1416e51cf29df7284fa99d0d/src/hoverExtension.ts)

export const rangeHover =
	new RequestType<HoverParams, Hover | null, void>("textDocument/hover");

export interface HoverParams extends WorkDoneProgressParams {
	textDocument: TextDocumentIdentifier;
	position?: Position;
	range?: Range;
}