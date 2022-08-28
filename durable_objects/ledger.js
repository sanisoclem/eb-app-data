
import { ledgerFetchMain } from '../output/Main';

export class Ledger {
  constructor(state) {
    this.state = state;
  }

  async fetch(request) {
    return await ledgerFetchMain(this.state.storage)(request)();
  }
}

