
export class URLs {

    constructor(location: any) {
        this._location = location
    }

    /**
     * websocket url e.g. for lsp 
     */
    getWebSocketUrl(path: string): string {
        if (this._location.protocol === 'https:') {
            return this.getUrl(path, 'wss:')
        }
        return this.getUrl(path, 'ws:')
    }

    /**
     *  http/https url
     */
    getUrl(path: string, protocol: string = this._location.protocol): string {
        let contextPath = this.getContextPath()
        let host = this._location.hostname
        return protocol + '//' + host + `:${this._location.port}` + contextPath + path
    }

    // compute the prefix before the ride segment, and reappend it.
    // this will make sure that any segments inserted by a proxy are retained. 
    getContextPath(): string {
        return this._location.pathname.substr(0, this._location.pathname.length - "/sadlmonaco".length)
    }

    private _location: Location;
}

export namespace Schemas {
    export const http: string = 'http';
    export const https: string = 'https';
}