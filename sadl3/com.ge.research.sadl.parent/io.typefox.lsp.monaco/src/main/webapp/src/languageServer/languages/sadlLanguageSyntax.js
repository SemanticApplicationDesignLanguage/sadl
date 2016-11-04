var conf = {

  keywords: [
	    'let', 'native'
	],

  operators: [
    '+', '-', '*', '/'
  ],

  autoClosingPairs: [
	  { open: '(', close: ')', notIn: ['string', 'comment'] },
    { open: '/**', close: '*/' },
  ],

  // The main tokenizer for our languages
  tokenizer: {
    root: [
      // keywords
      [/let|native/, { cases: { '@keywords': 'keyword' } }],
      [':', 'string' ],

      // whitespace
      { include: '@whitespace' },

      // delimiters and operators
      [/[{}()\[\]]/, '@brackets'],

      // numbers
      [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
      [/0[xX][0-9a-fA-F]+/, 'number.hex'],
      [/\d+/, 'number'],

      // delimiter: after number because of .\d floats
      [/[;,.]/, 'delimiter']
    ],

    comment: [
      [/[^\/*]+/, 'comment' ],
      [/\/\*/,    'comment', '@push' ],    // nested comment
      ["\\*/",    'comment', '@pop'  ],
      [/[\/*]/,   'comment' ]
    ],

    whitespace: [
      [/[ \t\r\n]+/, 'white'],
      [/\/\*/,       'comment', '@comment' ],
      [/\/\/.*$/,    'comment'],
    ],
  },
};

exports.conf = conf;
