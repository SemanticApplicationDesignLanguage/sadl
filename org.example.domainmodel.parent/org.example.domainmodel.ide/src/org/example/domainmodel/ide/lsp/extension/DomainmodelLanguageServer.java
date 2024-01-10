package org.example.domainmodel.ide.lsp.extension;

import java.lang.reflect.Field;
import java.util.List;

import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.SemanticTokensWithRegistrationOptions;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.xtext.ide.server.LanguageServerImpl;
import org.eclipse.xtext.ide.server.semantictokens.SemanticTokensService;

import com.google.inject.Inject;

public class DomainmodelLanguageServer extends LanguageServerImpl {

//	@Inject
//	private DomainModelSemanticTokensService semanticTokensService;
	
	public DomainmodelLanguageServer() {
	}

	@Override
	protected ServerCapabilities createServerCapabilities(InitializeParams params) {
		ServerCapabilities srvrCapabilities = super.createServerCapabilities(params);
		Field semanticTokensService;
		try {
			Class<?> superCls = this.getClass().getSuperclass();
			semanticTokensService = Class.forName(superCls.getCanonicalName()).getDeclaredField("semanticTokensService");
			//turning off access check with below method call
			semanticTokensService.setAccessible(true);
			Class<?> type = semanticTokensService.getType();
			System.out.println(type.getCanonicalName()); 
			SemanticTokensService semanticTokensServiceInst = (SemanticTokensService) semanticTokensService.get(this);
			if (semanticTokensServiceInst != null) {
				Field tokenTypes;
				tokenTypes = Class.forName(semanticTokensServiceInst.getClass().getCanonicalName()).getDeclaredField("tokenTypes");
				if (tokenTypes != null) {
					tokenTypes.setAccessible(true);
					List<String> tokenTypesInst = (List<String>) tokenTypes.get(semanticTokensServiceInst);
					tokenTypesInst.add(DomainModelHightlightingStyles.DATA_TYPE);
					tokenTypesInst.add(DomainModelHightlightingStyles.ENTITY);
				}
			}
		} catch (NoSuchFieldException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalArgumentException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		SemanticTokensWithRegistrationOptions semanticTokensWithRegistrationOptions = srvrCapabilities.getSemanticTokensProvider();
//		semanticTokensWithRegistrationOptions.setRange(true);
		return srvrCapabilities;
	}
	
//	@Override
//	public CompletableFuture<SemanticTokens> semanticTokensRange(final SemanticTokensParams params) {
//		return getRequestManager().runRead((cancelIndicator) -> semanticTokensFull(params, cancelIndicator));
//	}

}
