export interface LanguageDescription {
    languageId: string
    aliases: string[]
    mimeTypes: string[]
    fileExtensions: string[]
    highlightingConfiguration: string
    module?: any
}