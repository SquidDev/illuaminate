import { ExtensionContext, commands, workspace } from "vscode";
import { LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";

let client: LanguageClient | undefined;

const startServer = () => {
  // Spin up a new server
  const command = workspace.getConfiguration("illuaminate").get<string>("executable", "illuaminate-lsp");
  const serverOptions: ServerOptions = { command, args: ["--log=/home/squid/Documents/software/illuaminate/log.txt"] };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "lua" }],
    synchronize: {
      // Also notify the server about changes to .lua files.
      fileEvents: workspace.createFileSystemWatcher("**/.lua"),
    },
  };

  client = new LanguageClient("illuaminate", "illuaminate Language Server", serverOptions, clientOptions);
  client.start();
};

const stopServer = () => client ? client.stop() : Promise.resolve();

export const activate = (context: ExtensionContext) => {
  startServer();

  context.subscriptions.push(commands.registerCommand("illuaminate.restartServer", async () => {
    await stopServer();
    startServer();
  }));
};

export const deactivate = (): Thenable<void> | undefined => {
  if (!client) return undefined;
  return client.stop();
};
