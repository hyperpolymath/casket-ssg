// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * mdBook adapter - Documentation/book generator in Rust
 * https://rust-lang.github.io/mdBook/
 */

export const name = "mdBook";
export const language = "Rust";
export const description = "Create books from Markdown files, used for Rust documentation";

let connected = false;
let binaryPath = "mdbook";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(binaryPath, {
    args,
    cwd: cwd || Deno.cwd(),
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  const decoder = new TextDecoder();
  return {
    success: output.success,
    stdout: decoder.decode(output.stdout),
    stderr: decoder.decode(output.stderr),
    code: output.code,
  };
}

export async function connect() {
  try {
    const result = await runCommand(["--version"]);
    connected = result.success;
    return connected;
  } catch {
    connected = false;
    return false;
  }
}

export async function disconnect() {
  connected = false;
}

export function isConnected() {
  return connected;
}

export const tools = [
  {
    name: "mdbook_init",
    description: "Initialize a new mdBook",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new book" },
        title: { type: "string", description: "Book title" },
        force: { type: "boolean", description: "Overwrite existing files" },
      },
    },
    execute: async ({ path, title, force }) => {
      const args = ["init"];
      if (path) args.push(path);
      if (title) args.push("--title", title);
      if (force) args.push("--force");
      return await runCommand(args);
    },
  },
  {
    name: "mdbook_build",
    description: "Build the book",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to book root" },
        destDir: { type: "string", description: "Output directory" },
        open: { type: "boolean", description: "Open in browser after build" },
      },
    },
    execute: async ({ path, destDir, open }) => {
      const args = ["build"];
      if (path) args.push(path);
      if (destDir) args.push("--dest-dir", destDir);
      if (open) args.push("--open");
      return await runCommand(args);
    },
  },
  {
    name: "mdbook_serve",
    description: "Start mdBook development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to book root" },
        port: { type: "number", description: "Port number (default: 3000)" },
        hostname: { type: "string", description: "Hostname to bind to" },
        open: { type: "boolean", description: "Open browser automatically" },
      },
    },
    execute: async ({ path, port, hostname, open }) => {
      const args = ["serve"];
      if (path) args.push(path);
      if (port) args.push("--port", String(port));
      if (hostname) args.push("--hostname", hostname);
      if (open) args.push("--open");
      return await runCommand(args);
    },
  },
  {
    name: "mdbook_watch",
    description: "Watch for changes and rebuild",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to book root" },
        destDir: { type: "string", description: "Output directory" },
      },
    },
    execute: async ({ path, destDir }) => {
      const args = ["watch"];
      if (path) args.push(path);
      if (destDir) args.push("--dest-dir", destDir);
      return await runCommand(args);
    },
  },
  {
    name: "mdbook_clean",
    description: "Clean the build directory",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to book root" },
      },
    },
    execute: async ({ path }) => {
      const args = ["clean"];
      if (path) args.push(path);
      return await runCommand(args);
    },
  },
  {
    name: "mdbook_test",
    description: "Test code samples in the book",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to book root" },
      },
    },
    execute: async ({ path }) => {
      const args = ["test"];
      if (path) args.push(path);
      return await runCommand(args);
    },
  },
  {
    name: "mdbook_version",
    description: "Get mdBook version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
