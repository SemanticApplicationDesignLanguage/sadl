import * as lstypes from 'vscode-languageserver-types'
import * as protocol from 'vscode-languageclient/lib/protocol'
import { 
    MessageReader, MessageWriter,
    NotificationHandler, Logger, MessageConnection 
} from 'vscode-jsonrpc'

export class ConsoleLogger implements Logger {
	public error(message: string): void {
		console.error(message);
	}
	public warn(message: string): void {
		console.warn(message);
	}
	public info(message: string): void {
		console.info(message);
	}
	public log(message: string): void {
		console.log(message);
	}
}