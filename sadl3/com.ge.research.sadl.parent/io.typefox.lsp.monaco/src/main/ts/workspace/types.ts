export interface File {
    readonly uri: string;
    readonly directory: boolean;
    readonly children?: File[];
}

export interface FileContent {
    readonly value: string;
}
