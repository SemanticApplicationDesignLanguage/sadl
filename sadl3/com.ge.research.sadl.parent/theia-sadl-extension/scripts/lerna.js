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

// @ts-check
const path = require('path');

const lernaPath = path.resolve(__dirname, '..', 'node_modules', 'lerna', 'bin', 'lerna');

if (process.platform === 'win32') {
    console.log('Parallel lerna execution is disabled on Windows. Falling back to sequential execution with the \'--concurrency==1\' flag.');
    if (process.argv.indexOf('--concurrency==1') === -1) {
        process.argv.push('--concurrency==1');
    }
    const parallelIndex = process.argv.indexOf('--parallel');
    if (parallelIndex !== -1) {
        process.argv[parallelIndex] = '--stream';
    }
    console.log('Running lerna as: ' + process.argv.join(' '));
}
require(lernaPath);