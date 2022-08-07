export const doStringResponse = (resp) => new Response(resp);
export const doNotFoundResponse = (resp) => new Response(resp, { status: 404 });
export const doErrorResponse = (resp) => new Response(resp, { status: 500 });
export const doRequestGetBody = async (req) => await req.text();
export const doRequestGetMethodImpl = (req) => req.method;
export const doGetStateImpl = justFn => nothing => state => async (key) => {
  const r = state.get(key);
  if (r === undefined) return nothing;
  return justFn(r);
}