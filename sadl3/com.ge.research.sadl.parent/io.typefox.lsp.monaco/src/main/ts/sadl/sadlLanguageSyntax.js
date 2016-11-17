exports.conf = {

	brackets: [
		['{', '}', 'delimiter.curly'],
		['[', ']', 'delimiter.square'],
		['(', ')', 'delimiter.parenthesis']
	],

	keywords: [
		'uri', 'alias', 'version', 'note', 'import', 'as',
		'Equation', 'External', 'located', 'at', 'returns',
		'is', 'a', 'top-level', 'class', 'type', 'of',
		'property', 'the', 'same', 'not', 'and', 'are',
		'disjoint', 'relationship', 'only', 'if', 'with', 'has',
		'length', 'or', 'List', 'described', 'by', 'describes',
		'single', 'value', 'values', 'inverse', 'transitive',
		'symmetrical', 'annotation', 'default', 'subject',
		'must', 'be', 'one', 'can', 'always', 'least', 'most',
		'exactly', 'each', 'types', 'PI', 'true', 'false', 'known',
		'A', 'An', 'an', 'The', 'Expr', 'Rule', 'given', 'then',
		'Ask', 'Graph', 'select', 'distinct', 'asc', 'desc',
		'construct', 'where', 'ask', 'Test', 'Print', 'Deductions',
		'Model', 'Explain', 'Write', 'data', 'Read', 'using', 'from',
		'sublist', 'matching', 'unique', 'in', 'contains', 'does',
		'contain', 'element', 'before', 'after', 'e', 'None', 'count',
		'index', 'first', 'last', 'order by', 'string', 'boolean',
		'decimal', 'int', 'long', 'float', 'double', 'duration', 'dateTime',
		'time', 'date', 'gYearMonth', 'gYear', 'gMonthDay', 'gDay', 'gMonth',
		'hexBinary', 'base64Binary', 'anyURI', 'integer', 'negativeInteger',
		'nonNegativeInteger', 'positiveInteger', 'nonPositiveInteger',
		'unsignedByte', 'unsignedInt', 'anySimpleType'
	],

	operators: [
		'&&', '||', '>=', '<=', '>', '<', '+', '-', '*', '/', '%'
	],

	autoClosingPairs: [{
		open: '(',
		close: ')',
		notIn: ['string', 'comment']
	}, {
		open: '{',
		close: '}',
		notIn: ['string', 'comment']
	}, {
		open: '[',
		close: ']',
		notIn: ['string', 'comment']
	}, {
		open: '/**',
		close: '*/'
	}, ],

	escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

	// The main tokenizer for our languages
	tokenizer: {
		root: [

			// whitespace
			{
				include: '@whitespace'
			},

			[/[a-zA-Z_][\w_]*('*)/, {
				cases: {
					'@keywords': 'keyword',
					'@default': 'identifier'
				}
			}],

			// delimiters and operators
			[/[{}()\[\]]/, '@brackets'],

			// numbers
			[/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
			[/0[xX][0-9a-fA-F]+/, 'number.hex'],
			[/\d+/, 'number'],

			// delimiter: after number because of .\d floats
			[/[;,.]/, 'delimiter'],

			// strings
			[/"([^"\\]|\\.)*$/, 'string.invalid'], // non-terminated string
			[/"/, 'string', '@string'],
		],

		whitespace: [
			[/[ \t\r\n]+/, 'white'],
			[/\/\*/, 'comment', '@comment'],
			[/\/\/.*$/, 'comment'],
		],

		comment: [
			[/[^\/*]+/, 'comment'],
			// [/\/\*/, 'comment', '@push' ],    // nested comment not allowed :-(
			[/\/\*/, 'comment.invalid'],
			["\\*/", 'comment', '@pop'],
			[/[\/*]/, 'comment']
		],

		string: [
			[/[^\\"]+/, 'string'],
			[/@escapes/, 'string.escape'],
			[/\\./, 'string.escape.invalid'],
			[/"/, {
				token: 'string.quote',
				bracket: '@close',
				next: '@pop'
			}]
		]

	}
};

exports.language = exports.conf;
