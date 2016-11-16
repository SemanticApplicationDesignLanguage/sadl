import {
    URL
} from '../utils/network';

export function getRootPath(url: URL): Thenable<string> {
    return new Promise<string>((resolve, reject) => {
        const request = new XMLHttpRequest()
        request.open('GET', url.toString());
        request.onload = () => {
            if (request.status === 200) {
                resolve(request.responseText);
            } else {
                reject(new Error('Error while getting root path for language server. Return status was: ' + request.status));
            }
        }
        request.send();
    });
}