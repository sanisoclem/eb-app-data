export const doStringResponse = (resp) => new Response(resp);
export const doErrorResponse = (resp) => new Response(resp, { status: 500 });
export const doRequestGetBody = async (req) => await req.text();
export const doRequestGetMethodImpl = (req) => req.method;
