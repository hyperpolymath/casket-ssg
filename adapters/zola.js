// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Zola adapter - Fast static site generator in Rust
 * https://www.getzola.org/
 */

export const name = "Zola";
export const language = "Rust";
export const description = "Fast static site generator written in Rust with built-in Sass compilation and syntax highlighting";

let connected = false;
let binaryPath = "zola";

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
    name: "zola_init",
    description: "Initialize a new Zola site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
        force: { type: "boolean", description: "Overwrite existing directory" },
      },
      required: ["path"],
    },
    execute: async ({ path, force }) => {
      const args = ["init", path];
      if (force) args.push("--force");
      return await runCommand(args);
    },
  },
  {
    name: "zola_build",
    description: "Build the Zola site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        baseUrl: { type: "string", description: "Base URL for the site" },
        outputDir: { type: "string", description: "Output directory" },
        drafts: { type: "boolean", description: "Include drafts" },
      },
    },
    execute: async ({ path, baseUrl, outputDir, drafts }) => {
      const args = ["build"];
      if (baseUrl) args.push("--base-url", baseUrl);
      if (outputDir) args.push("--output-dir", outputDir);
      if (drafts) args.push("--drafts");
      return await runCommand(args, path);
    },
  },
  {
    name: "zola_serve",
    description: "Start Zola development server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number (default: 1111)" },
        interface: { type: "string", description: "Interface to bind to" },
        drafts: { type: "boolean", description: "Include drafts" },
        openBrowser: { type: "boolean", description: "Open browser automatically" },
      },
    },
    execute: async ({ path, port, interface: iface, drafts, openBrowser }) => {
      const args = ["serve"];
      if (port) args.push("--port", String(port));
      if (iface) args.push("--interface", iface);
      if (drafts) args.push("--drafts");
      if (openBrowser) args.push("--open");
      return await runCommand(args, path);
    },
  },
  {
    name: "zola_check",
    description: "Check the site for errors",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        drafts: { type: "boolean", description: "Include drafts" },
      },
    },
    execute: async ({ path, drafts }) => {
      const args = ["check"];
      if (drafts) args.push("--drafts");
      return await runCommand(args, path);
    },
  },
  {
    name: "zola_version",
    description: "Get Zola version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
