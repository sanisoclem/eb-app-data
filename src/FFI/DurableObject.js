// exported directly
export const doStringResponse = (resp) => (status) => new Response(resp, { status })

// wrapped functions
export const doRequestGetMethod = (req) => req.method;
export const doRequestGetPath = (req) => new URL(req.url).pathname.slice(1);
export const doRequestGetParamImpl = justFn => nothing => req => key => {
  const params = (new URL(req.url)).searchParams
  const p = params.get(key)
  if (p === undefined || p === null || p === '') return nothing;
  return justFn(p);
}

export const doRequestGetBodyImpl = (req) => () => req.text();
export const dogetDurableStateImpl = justFn => nothing => state => key => async () => {
  const r = await state.get(key);
  //throw new Error(`Key: ${key} bpdy: ${JSON.stringify(r)}`)
  if (r === undefined) return nothing;
  return justFn(r);
}
export const dogetDurableStateByPrefixImpl = state => prefix => async () => {
  var ret = await state.list({ prefix });
  //throw new Error(`Key: ${prefix} bpdy: ${JSON.stringify(Array.from(ret.entries()).map(([k,v]) => ({ key: k, value: v })))}`)
  return Array.from(ret.entries()).map(([k,v]) => ({ key: k, value: v }));
}
export const doputDurableStateImpl = state => key => value => () => state.put(key, value);
export const dodeleteDurableStateImpl = state => key => () => state.delete(key);
export const doBatchStateImpl = state => deletes => puts => async () => {
  let lastPromise = null;
  if (deletes.length)
    lastPromise = state.delete(deletes);

  puts.forEach(p => {
    lastPromise = state.put(p.id, p.document);
  });

  if (lastPromise !== null)
    await lastPromise;
}