export * from './ledger'

export default {
  fetch() {
    return new Response(
      "This Worker creates the Durable Objects."
    );
  },
};