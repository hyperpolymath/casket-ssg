// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Babashka adapter - Fast Clojure scripting for static sites
 * https://babashka.org/
 */

export const name = "Babashka";
export const language = "Clojure";
export const description = "Fast Clojure scripting runtime, usable for static site generation";

let connected = false;
let bbPath = "bb";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(bbPath, {
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
    name: "babashka_run",
    description: "Run a Babashka script",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Working directory" },
        script: { type: "string", description: "Script file to run" },
      },
      required: ["script"],
    },
    execute: async ({ path, script }) => await runCommand([script], path),
  },
  {
    name: "babashka_eval",
    description: "Evaluate Clojure expression",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Working directory" },
        expr: { type: "string", description: "Clojure expression" },
      },
      required: ["expr"],
    },
    execute: async ({ path, expr }) => await runCommand(["-e", expr], path),
  },
  {
    name: "babashka_tasks",
    description: "List available tasks",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Working directory" },
      },
    },
    execute: async ({ path }) => await runCommand(["tasks"], path),
  },
  {
    name: "babashka_task",
    description: "Run a task from bb.edn",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Working directory" },
        task: { type: "string", description: "Task name" },
      },
      required: ["task"],
    },
    execute: async ({ path, task }) => await runCommand([task], path),
  },
  {
    name: "babashka_nrepl",
    description: "Start nREPL server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Working directory" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["nrepl-server"];
      if (port) args.push(String(port));
      return await runCommand(args, path);
    },
  },
  {
    name: "babashka_version",
    description: "Get Babashka version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
