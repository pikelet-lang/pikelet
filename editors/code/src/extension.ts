import { ExtensionContext, workspace } from 'vscode';
import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions
} from 'vscode-languageclient';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // Get the workspace's configuration
    const config = workspace.getConfiguration('pikelet');

    // The server is implemented in node
    const serverExecutable: Executable = {
        command: config.get('languageServer.path', 'pikelet'),
        args: ['language-server'],
        options: {
            // Enable backtraces so that we can get better reporting for
            // any inevitable bugs we might encounter...
            env: {
                RUST_BACKTRACE: 1
            }
        }
    };

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    const serverOptions: ServerOptions = {
        run: serverExecutable,
        debug: serverExecutable
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for pikelet documents
        documentSelector: [
            {
                scheme: 'file',
                language: 'pikelet'
            }
        ]
    };

    // Create the language client and start the client.
    client = new LanguageClient(
        'pikelet-language-server',
        'Pikelet Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
