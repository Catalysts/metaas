/*
 * StatementBuilder.java
 * 
 * Copyright (c) 2006-2007 David Holroyd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.co.badgersinfoil.metaas.impl;

import org.asdt.core.internal.antlr.AS3Parser;
import uk.co.badgersinfoil.metaas.ActionScriptFactory;
import uk.co.badgersinfoil.metaas.SyntaxException;
import uk.co.badgersinfoil.metaas.dom.Statement;
import uk.co.badgersinfoil.metaas.impl.antlr.LinkedListTree;


/**
 * Utility for building {@link Statement} implementations wrapping statement
 * nodes in the AST.
 */
public class StatementBuilder {

	/**
	 * Build a Statement implementation (ASTAS*Statement) appropriate for
	 * the type of the given AST node.
	 */
	public static Statement build(LinkedListTree ast) {
		switch (ast.getType()) {
			case AS3Parser.BLOCK:
				return new ASTStatementList(ast);
			case AS3Parser.DO:
				return new ASTASDoWhileStatement(ast);
			case AS3Parser.EXPR_STMNT:
				return new ASTASExpressionStatement(ast);
			case AS3Parser.FOR_EACH:
				return new ASTASForEachInStatement(ast);
			case AS3Parser.FOR_IN:
				return new ASTASForInStatement(ast);
			case AS3Parser.FOR:
				return new ASTASForStatement(ast);
			case AS3Parser.IF:
				return new ASTASIfStatement(ast);
			case AS3Parser.SWITCH:
				return new ASTASSwitchStatement(ast);
			case AS3Parser.CONST:
			case AS3Parser.VAR:
				return new ASTASDeclarationStatement(ast);
			case AS3Parser.WHILE:
				return new ASTASWhileStatement(ast);
			case AS3Parser.WITH:
				return new ASTASWithStatement(ast);
			case AS3Parser.RETURN:
				return new ASTASReturnStatement(ast);
			case AS3Parser.SUPER:
				return new ASTASSuperStatement(ast);
			case AS3Parser.BREAK:
				return new ASTASBreakStatement(ast);
			case AS3Parser.TRY:
				return new ASTASTryStatement(ast);
			case AS3Parser.DEFAULT_XML_NAMESPACE:
				return new ASTASDefaultXMLNamespaceStatement(ast);
			case AS3Parser.CONTINUE:
				return new ASTASContinueStatement(ast);
			case AS3Parser.THROW:
				return new ASTASThrowStatement(ast);
			default:
				throw new SyntaxException("Unsupported statement node type: "+ASTUtils.tokenName(ast)+" in "+ActionScriptFactory.str(ASTUtils.stringifyNode(ast)));
		}
	}
}
