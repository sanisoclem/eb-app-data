// exported directly
export const doStringResponse = (resp) => (status) => new Response(resp, { status })

// wrapped functions
export const doRequestGetMethodImpl = (req) => req.method;
export const doRequestGetBodyImpl = (req) => () => req.text();
export const doGetStateImpl = justFn => nothing => state => key => () => {
  const r = state.get(key);
  if (r === undefined) return nothing;
  return justFn(r);
}
export const doPutStateImpl = state => key => value => () => state.put(key, value);
//export const doBatchState = state =>