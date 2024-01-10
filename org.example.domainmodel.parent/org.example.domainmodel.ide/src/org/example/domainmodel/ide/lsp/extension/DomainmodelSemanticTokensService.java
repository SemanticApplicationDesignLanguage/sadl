package org.example.domainmodel.ide.lsp.extension;

import org.eclipse.xtext.ide.server.semantictokens.SemanticTokensService;

public class DomainmodelSemanticTokensService extends SemanticTokensService {

	@Override
	protected void addTokenTypes() {
		super.addTokenTypes();
		getTokenTypes().add(DomainmodelHighlightingStyles.DATA_TYPE);
		getTokenTypes().add(DomainmodelHighlightingStyles.ENTITY);
	}
}
