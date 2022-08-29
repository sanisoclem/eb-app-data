export * from "./ledger";

export default {
  async fetch(request, env) {
    if (request.method === "POST") {
      const id = env.EB_LEDGER_DO.newUniqueId();
      return new Response(id.toString());
    } else if (request.method === "PUT") {
      const url = new URL(request.url);
      const doId = env.EB_LEDGER_DO.idFromString(url.pathname.slice(1));
      const stub = env.EB_LEDGER_DO.get(doId);
      return stub.fetch(request);
    } else if (request.method === "GET") {
      const url = new URL(request.url);
      const segments = url.pathname.slice(1).split("/")
      const doId = env.EB_LEDGER_DO.idFromString(segments[0]);
      const stub = env.EB_LEDGER_DO.get(doId);
      url.pathname = `/${segments[1]}`
      return stub.fetch(new Request(url));
    } else {
      return new Response("Method Not Allowed", { status: 405 });
    }
  },
};
