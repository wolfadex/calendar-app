import { v4 } from "https://deno.land/std@0.74.0/uuid/mod.ts";
import {
  listenAndServe,
  serve,
  ServerRequest,
} from "https://deno.land/std@0.74.0/http/server.ts";
import {
  createRouter,
  AugmentedRequest,
  createRouteMap,
  textResponse,
  jsonResponse,
  NotFoundError,
} from "https://deno.land/x/reno@v1.3.5/reno/mod.ts";
import { compose } from "https://deno.land/x/compose@1.3.2/index.js";

interface FakeDB {
  events: FakeTable;
}

interface FakeTable {
  [key: string]: any;
}

const fakeDB: FakeDB = {
  events: {},
};

const apiRoutes = createRouteMap([
  [
    "/event",
    async (req: AugmentedRequest) => {
      switch (req.method) {
        case "POST": {
          fakeDB.events[v4.generate()] = req.body;
          return jsonResponse({}, {}, 201);
        }
        case "GET": {
          const events = Object.entries(fakeDB.events).map(([id, data]) => ({
            ...data,
            id,
          }));
          return jsonResponse(events);
        }
        default:
          return methodNotAllowed();
      }
    },
  ],
]);

const routes = createRouteMap([
  ["/", () => textResponse("Hello, Server!")],
  ["/api/*", createRouter(apiRoutes)],
]);

const router = createRouter(routes);

const mapToErrorResponse = (error: Error) =>
  error instanceof NotFoundError ? notFound(error) : serverError(error);

const serverError = (error: Error) => createErrorResponse(500, error);
const notFound = (error: Error) => createErrorResponse(404, error);
const methodNotAllowed = () =>
  createErrorResponse(405, {
    message: "Method Not Allowed",
    name: "Method Not Allowed",
  });

function createErrorResponse(status: number, { message }: Error) {
  return textResponse(message, {}, status);
}

async function main(port = 8001) {
  const server = serve({ port });
  console.log(`Listening for requests on port ${port}`);
  for await (const req of server) {
    // await listenAndServe(`:${port}`, async (req: ServerRequest) => {
    try {
      //   const corsOptions = Cors.produceCorsOptions({});
      //   const originDelegate = Cors.produceOriginDelegate(corsOptions);

      //   if (originDelegate) {
      //     const requestMethod = req.method;
      //     const getRequestHeader = (headerKey: string) =>
      //       req.headers.get(headerKey);
      //     // const getResponseHeader = (headerKey: string) =>
      //     //   response.headers.get(headerKey);
      //     // const setResponseHeader = (headerKey: string, headerValue: string) =>
      //     //   response.headers.set(headerKey, headerValue);
      //     // const setStatus = (statusCode: number) =>
      //     //   (response.status = statusCode);

      //     const origin = await originDelegate(getRequestHeader("origin"));

      //     if (origin) {
      //       console.log("origin request");
      //       // corsOptions.origin = origin;

      //       // return new Cors({
      //       //   corsOptions,
      //       //   requestMethod,
      //       //   getRequestHeader,
      //       //   getResponseHeader,
      //       //   setResponseHeader,
      //       //   setStatus,
      //       //   next,
      //       // }).configureHeaders();
      //     }
      //   }
      console.log(new Date().toJSON(), req.method, req.url);
      const res = await router(req);
      return req.respond(res);
    } catch (error) {
      return req.respond(mapToErrorResponse(error));
    }
    // });
  }
}

export { main };
