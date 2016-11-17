export namespace Protocol {
    export const http = 'http:';
    export const https = 'https:';
    export const ws = 'ws:';
    export const wss = 'wss:';
}

export const wsProtocol = location.protocol === Protocol.https ? Protocol.wss : Protocol.ws;

export class URL {

    constructor(protected props: URL.Props) {}

    toString(): string {
        const protocol = this.props.protocol || location.protocol;
        const port = this.props.port || location.port;
        const host = this.props.host || location.hostname;
        const url = `${protocol}//${host}:${port}${this.contextPath}`;
        return this.props.path ? url + '/' + this.props.path : url;
    }

    get contextPath(): string {
        const contextPath = location.pathname.replace(/\/$/, "");
        const basePath = this.props.basePath;
        if (basePath) {
            const index = contextPath.lastIndexOf(basePath);
            if (index === -1) {
                return contextPath + '/' + basePath;
            }
            return location.pathname.substr(0, index + basePath.length);
        }
        return contextPath;
    }

}

export namespace URL {
    export interface Props {
        protocol?: string
        host?: string
        port?: number
        basePath?: string
        path?: string
    }
}
