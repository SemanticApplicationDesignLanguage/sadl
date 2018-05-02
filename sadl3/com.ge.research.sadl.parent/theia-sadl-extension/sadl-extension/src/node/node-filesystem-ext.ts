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

import * as fs from 'fs-extra';
import { injectable } from 'inversify';
import { FileUri } from '@theia/core/lib/node/file-uri';
import { FileSystemExt } from '../common/filesystem-ext';

@injectable()
export class NodeFileSystemExt implements FileSystemExt {

    async canWrite(uri: string): Promise<boolean> {
        try {
            const path = FileUri.fsPath(uri);
            await fs.access(path, fs.constants.W_OK);
            return true;
        } catch {
            return false;
        }
    }

}