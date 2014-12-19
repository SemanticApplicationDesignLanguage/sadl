package com.ge.research.sadl.ui.editor;

import java.io.IOException;
import java.net.URISyntaxException;

import javax.inject.Inject;

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.URLHyperlink;
import org.eclipse.jface.text.hyperlink.URLHyperlinkDetector;

import com.ge.research.sadl.builder.ConfigurationManagerForIDE;
import com.ge.research.sadl.builder.SadlModelManager;
import com.ge.research.sadl.reasoner.ConfigurationException;

public class SadlURLHyperlinkDetector extends URLHyperlinkDetector {
	@Inject
	private SadlModelManager visitor;
	
	@Override
	public IHyperlink[] detectHyperlinks(ITextViewer textViewer,
			IRegion region, boolean canShowMultipleHyperlinks) {
		IHyperlink[] hyperlinks = super.detectHyperlinks(textViewer, region,
				canShowMultipleHyperlinks);
		
		for (int i=0; i<hyperlinks.length; i++) {
			IHyperlink hyperlink = hyperlinks[i];
			if (hyperlink instanceof URLHyperlink) {
				URLHyperlink urlHyperlink = (URLHyperlink) hyperlink;
				String publicUri = urlHyperlink.getURLString();
				ConfigurationManagerForIDE cmgr = null;
				try {
					cmgr = visitor.getConfigurationMgr(publicUri);
					String altUrl = cmgr.getAltUrlFromPublicUri(publicUri);
					urlHyperlink = new URLHyperlink(hyperlink.getHyperlinkRegion(), altUrl);
					// replace hyperlink with mapped one
					hyperlinks[i] = urlHyperlink;
				} catch (ConfigurationException | URISyntaxException
						| IOException e) {
				}
			}
		}
		return hyperlinks;
	}
}
