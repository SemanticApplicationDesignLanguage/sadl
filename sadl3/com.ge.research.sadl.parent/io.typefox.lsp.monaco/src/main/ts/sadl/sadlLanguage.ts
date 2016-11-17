import {
    LanguageDescription
} from '../client';

export const sadlLanguage: LanguageDescription = {
    languageId: 'com.ge.research.sadl.SADL',
    aliases: ['sadl'],
    mimeTypes: [],
    fileExtensions: ["sadl"],
    highlightingConfiguration: '',
    module: require('./sadlLanguageSyntax')
};
