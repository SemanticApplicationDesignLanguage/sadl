import {
    File
} from '../workspace';

import {
    Explorer
} from './Explorer';

export interface IExplorerPart {
    open(file: File | null): void;
}

export class ExplorerPart implements IExplorerPart {

    explorer: Explorer | null;

    open(file: File | null): void {
        if (this.explorer && file) {
            this.explorer.open(file);
        }
    }

}