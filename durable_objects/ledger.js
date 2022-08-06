
import { fetchMain } from '../output/Ledger';

export class Ledger {
  constructor(state) {
    this.state = state;
  }

  async fetch(request) {
    await fetchMain(this.state)(request);
  }
}

