// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Hakyll adapter - Static site generator in Haskell
 * https://jaspervdj.be/hakyll/
 */

export const name = "Hakyll";
export const language = "Haskell";
export const description = "Haskell library for generating static sites with Pandoc support";

let connected = false;
let stackPath = "stack";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(stackPath, {
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

async function runSiteCommand(args, cwd = null) {
  // Hakyll sites are typically run via stack exec site
  return await runCommand(["exec", "site", "--", ...args], cwd);
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
    name: "hakyll_init",
    description: "Initialize a new Hakyll site (using stack template)",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Project name" },
        path: { type: "string", description: "Path for the project" },
      },
      required: ["name"],
    },
    execute: async ({ name, path }) => {
      return await runCommand(["new", name, "hakyll-template"], path);
    },
  },
  {
    name: "hakyll_build",
    description: "Build the Hakyll site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runSiteCommand(["build"], path),
  },
  {
    name: "hakyll_watch",
    description: "Start Hakyll watch server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
        host: { type: "string", description: "Host to bind to" },
      },
    },
    execute: async ({ path, port, host }) => {
      const args = ["watch"];
      if (port) args.push("--port", String(port));
      if (host) args.push("--host", host);
      return await runSiteCommand(args, path);
    },
  },
  {
    name: "hakyll_clean",
    description: "Clean the build cache",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runSiteCommand(["clean"], path),
  },
  {
    name: "hakyll_rebuild",
    description: "Clean and rebuild the site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runSiteCommand(["rebuild"], path),
  },
  {
    name: "hakyll_check",
    description: "Check for broken links",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        internal: { type: "boolean", description: "Check internal links only" },
      },
    },
    execute: async ({ path, internal }) => {
      const args = ["check"];
      if (internal) args.push("--internal-links");
      return await runSiteCommand(args, path);
    },
  },
  {
    name: "hakyll_deploy",
    description: "Deploy the site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runSiteCommand(["deploy"], path),
  },
  {
    name: "hakyll_version",
    description: "Get Stack/Hakyll version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
