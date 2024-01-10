package org.example.domainmodel.ide.lsp.extension;

import org.eclipse.xtext.ide.server.semantictokens.SemanticTokensService;

public class DomainModelSemanticTokensService extends SemanticTokensService {

	@Override
	protected void addTokenTypes() {
		super.addTokenTypes();
		getTokenTypes().add(DomainModelHightlightingStyles.DATA_TYPE);
		getTokenTypes().add(DomainModelHightlightingStyles.ENTITY);
	}
}
