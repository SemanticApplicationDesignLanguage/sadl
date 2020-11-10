/************************************************************************
 * Copyright 2007-2018 General Electric Company, All Rights Reserved
 * 
 * Project: SADL
 * 
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 * 
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 * 
 ***********************************************************************/

import { inject, injectable } from 'inversify';
import { DidChangeWatchedFilesNotification, FileChangeType } from '@theia/languages/lib/browser';
import { DisposableCollection } from '@theia/core/lib/common/disposable';
import { FileSystemWatcher } from '@theia/filesystem/lib/browser/filesystem-watcher';
import { BaseLanguageClientContribution, LanguageClientFactory, Languages, Workspace, ILanguageClient, FileSystemWatcher as ServicesFileSystemWatcher, FileEvent } from '@theia/languages/lib/browser';
import { SadlEditorColoringService } from './sadl-editor-coloring-service';
import { SadlColoring } from './protocol/sadl-coloring';

@injectable()
export class SadlLanguageClientContribution extends BaseLanguageClientContribution {

    protected toDisposeAfterClientReady: DisposableCollection = new DisposableCollection();
    protected queuedFileEvents: FileEvent[] = [];

    readonly id = 'sadl'
    readonly name = 'SADL'

    constructor(
        @inject(Workspace) protected workspace: Workspace,
        @inject(Languages) protected languages: Languages,
        @inject(LanguageClientFactory) protected languageClientFactory: LanguageClientFactory,
        @inject(SadlEditorColoringService) protected editorColoringService: SadlEditorColoringService,
        @inject(FileSystemWatcher) protected readonly fileSystemWatcher: FileSystemWatcher
    ) {
        super(workspace, languages, languageClientFactory)
    }

    protected get globPatterns() {
        return [
            '**/*.sadl',
            '**/*.owl',
        ];
    }

    protected onReady(languageClient: ILanguageClient): void {
        languageClient.onNotification(SadlColoring.TYPE, this.editorColoringService.updateColoringInformation.bind(this.editorColoringService));
        // Notify the LS about any file changes that happened during the initialization.
        if (this.queuedFileEvents.length > 0) {
            languageClient.sendNotification(DidChangeWatchedFilesNotification.type, {
                changes: this.queuedFileEvents
            });
        }
        this.queuedFileEvents.length = 0;
        this.toDisposeAfterClientReady.dispose();
        super.onReady(languageClient);
    }

    // ts-lint:disable-next-line:
    waitForActivation(): Promise<any> {
        // To ensure eager activation. We do not wait until the first file open.
        return this.workspace.ready;
    }

    createFileEvents(): ServicesFileSystemWatcher[] {
        const watchers = super.createFileEvents();
        // Any file change events must be queued and send it to the server once the client is connected.
        // Why? Because the build happen on LS initialize, during the build we "create new files" that has to be rebuild.
        // So we need to notify the LS about the file changes. During the LS initialize it is not possible, because the client is not ready.
        this.toDisposeAfterClientReady.pushAll(watchers.map(watcher => watcher.onDidChange(uri => this.queuedFileEvents.push({ uri: uri.toString(), type: FileChangeType.Changed }))));
        this.toDisposeAfterClientReady.pushAll(watchers.map(watcher => watcher.onDidCreate(uri => this.queuedFileEvents.push({ uri: uri.toString(), type: FileChangeType.Created }))));
        this.toDisposeAfterClientReady.pushAll(watchers.map(watcher => watcher.onDidDelete(uri => this.queuedFileEvents.push({ uri: uri.toString(), type: FileChangeType.Deleted }))));
        return watchers;
    }

}
