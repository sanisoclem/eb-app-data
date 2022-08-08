
import { ledgerFetchMain } from '../output/Main';

export class Ledger {
  constructor(state) {
    this.state = state;
  }

  async fetch(request) {
    await ledgerFetchMain(this.state)(request)();
  }
}

