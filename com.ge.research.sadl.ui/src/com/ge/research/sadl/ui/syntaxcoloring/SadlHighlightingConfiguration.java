package com.ge.research.sadl.ui.syntaxcoloring;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.xtext.ui.editor.syntaxcoloring.DefaultHighlightingConfiguration;
import org.eclipse.xtext.ui.editor.syntaxcoloring.IHighlightingConfigurationAcceptor;
import org.eclipse.xtext.ui.editor.utils.TextStyle;

public class SadlHighlightingConfiguration extends DefaultHighlightingConfiguration {

    public static final String URI_ID = "uri";
    public static final String CLASS_ID = "class";
    public static final String INSTANCE_ID = "instance";
    public static final String VARIABLE_ID = "variable";
    public static final String DATA_PROPERTY_ID = "dataProperty";
    public static final String OBJECT_PROPERTY_ID = "objectProperty";
    public static final String ANNOTATION_PROPERTY_ID = "annotationProperty";
    public static final String RDFDATATYPE_ID = "rdfDataType";

    @Override
    public void configure(IHighlightingConfigurationAcceptor acceptor) {
        super.configure(acceptor);

        acceptor.acceptDefaultHighlighting(URI_ID, "URI", uriTextStyle());
        acceptor.acceptDefaultHighlighting(CLASS_ID, "Class", classTextStyle());
        acceptor.acceptDefaultHighlighting(INSTANCE_ID, "Instance", instanceTextStyle());
        acceptor.acceptDefaultHighlighting(VARIABLE_ID, "Variable", variableTextStyle());
        acceptor.acceptDefaultHighlighting(DATA_PROPERTY_ID, "Data Property", dataPropertyTextStyle());
        acceptor.acceptDefaultHighlighting(OBJECT_PROPERTY_ID, "Object Property", objectPropertyTextStyle());
        acceptor.acceptDefaultHighlighting(ANNOTATION_PROPERTY_ID, "Annotation Property", annotationPropertyTextStyle());
        acceptor.acceptDefaultHighlighting(RDFDATATYPE_ID, "RDF Data Type", uriTextStyle());
    } 

    // SADL V1 used SWT.COLOR_BLACK and SWT.BOLD.
    public TextStyle uriTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(0, 0, 0));
        textStyle.setStyle(SWT.BOLD);
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_DARK_BLUE and SWT.BOLD.
    public TextStyle classTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(0, 0, 128));
        textStyle.setStyle(SWT.BOLD);
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_BLUE.
    public TextStyle instanceTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(0, 0, 255));
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_MAGENTA and SWT.BOLD.
    public TextStyle variableTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(255, 0, 255));
        textStyle.setStyle(SWT.BOLD);
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_DARK_GREEN and SWT.BOLD.
    public TextStyle dataPropertyTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(0, 128, 0));
        textStyle.setStyle(SWT.BOLD);
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_DARK_GREEN and SWT.BOLD.
    public TextStyle objectPropertyTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(0, 128, 0));
        textStyle.setStyle(SWT.BOLD);
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_DARK_GREEN.
    public TextStyle annotationPropertyTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(0, 128, 0));
        return textStyle;
    }

    // SADL V1 used a light blue background. 
    @Override
    public TextStyle numberTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setBackgroundColor(new RGB(230, 240, 255));
        return textStyle;
    }

    // SADL V1 used a light blue background. 
    @Override
    public TextStyle stringTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setBackgroundColor(new RGB(230, 240, 255));
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_DARY_GRAY and SWT.ITALIC.
    @Override
    public TextStyle commentTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(128, 128, 128));
        textStyle.setStyle(SWT.ITALIC);
        return textStyle;
    }

    // SADL V1 used SWT.COLOR_DARK_MAGENTA.
    @Override
    public TextStyle keywordTextStyle() {
        TextStyle textStyle = defaultTextStyle().copy();
        textStyle.setColor(new RGB(128, 0, 128));
        return textStyle;
    }

}
