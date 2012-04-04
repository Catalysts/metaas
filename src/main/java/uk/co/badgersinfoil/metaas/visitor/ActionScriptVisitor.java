/*
 * ActionScriptVisitor.java
 * 
 * Copyright (c) 2008 David Holroyd
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

package uk.co.badgersinfoil.metaas.visitor;

import uk.co.badgersinfoil.metaas.dom.ASArg;
import uk.co.badgersinfoil.metaas.dom.ASArrayAccessExpression;
import uk.co.badgersinfoil.metaas.dom.ASArrayLiteral;
import uk.co.badgersinfoil.metaas.dom.ASAssignmentExpression;
import uk.co.badgersinfoil.metaas.dom.ASBinaryExpression;
import uk.co.badgersinfoil.metaas.dom.ASBlock;
import uk.co.badgersinfoil.metaas.dom.ASBooleanLiteral;
import uk.co.badgersinfoil.metaas.dom.ASBreakStatement;
import uk.co.badgersinfoil.metaas.dom.ASCatchClause;
import uk.co.badgersinfoil.metaas.dom.ASClassType;
import uk.co.badgersinfoil.metaas.dom.ASCompilationUnit;
import uk.co.badgersinfoil.metaas.dom.ASConditionalExpression;
import uk.co.badgersinfoil.metaas.dom.ASContinueStatement;
import uk.co.badgersinfoil.metaas.dom.ASDeclarationStatement;
import uk.co.badgersinfoil.metaas.dom.ASDefaultXMLNamespaceStatement;
import uk.co.badgersinfoil.metaas.dom.ASDescendantExpression;
import uk.co.badgersinfoil.metaas.dom.ASDoWhileStatement;
import uk.co.badgersinfoil.metaas.dom.ASExpressionAttribute;
import uk.co.badgersinfoil.metaas.dom.ASExpressionStatement;
import uk.co.badgersinfoil.metaas.dom.ASField;
import uk.co.badgersinfoil.metaas.dom.ASFieldAccessExpression;
import uk.co.badgersinfoil.metaas.dom.ASFilterExpression;
import uk.co.badgersinfoil.metaas.dom.ASFinallyClause;
import uk.co.badgersinfoil.metaas.dom.ASForEachInStatement;
import uk.co.badgersinfoil.metaas.dom.ASForInStatement;
import uk.co.badgersinfoil.metaas.dom.ASForStatement;
import uk.co.badgersinfoil.metaas.dom.ASFunctionExpression;
import uk.co.badgersinfoil.metaas.dom.ASIfStatement;
import uk.co.badgersinfoil.metaas.dom.ASIntegerLiteral;
import uk.co.badgersinfoil.metaas.dom.ASInterfaceType;
import uk.co.badgersinfoil.metaas.dom.ASInvocationExpression;
import uk.co.badgersinfoil.metaas.dom.ASMetaTag;
import uk.co.badgersinfoil.metaas.dom.ASMethod;
import uk.co.badgersinfoil.metaas.dom.ASNewExpression;
import uk.co.badgersinfoil.metaas.dom.ASNullLiteral;
import uk.co.badgersinfoil.metaas.dom.ASObjectLiteral;
import uk.co.badgersinfoil.metaas.dom.ASPackage;
import uk.co.badgersinfoil.metaas.dom.ASPostfixExpression;
import uk.co.badgersinfoil.metaas.dom.ASPrefixExpression;
import uk.co.badgersinfoil.metaas.dom.ASPropertyAttribute;
import uk.co.badgersinfoil.metaas.dom.ASRegexpLiteral;
import uk.co.badgersinfoil.metaas.dom.ASReturnStatement;
import uk.co.badgersinfoil.metaas.dom.ASSimpleNameExpression;
import uk.co.badgersinfoil.metaas.dom.ASStarAttribute;
import uk.co.badgersinfoil.metaas.dom.ASStringLiteral;
import uk.co.badgersinfoil.metaas.dom.ASSuperStatement;
import uk.co.badgersinfoil.metaas.dom.ASSwitchCase;
import uk.co.badgersinfoil.metaas.dom.ASSwitchDefault;
import uk.co.badgersinfoil.metaas.dom.ASSwitchStatement;
import uk.co.badgersinfoil.metaas.dom.ASThrowStatement;
import uk.co.badgersinfoil.metaas.dom.ASTryStatement;
import uk.co.badgersinfoil.metaas.dom.ASUndefinedLiteral;
import uk.co.badgersinfoil.metaas.dom.ASVarDeclarationFragment;
import uk.co.badgersinfoil.metaas.dom.ASWhileStatement;
import uk.co.badgersinfoil.metaas.dom.ASWithStatement;
import uk.co.badgersinfoil.metaas.dom.ASXMLLiteral;
import uk.co.badgersinfoil.metaas.dom.ASObjectLiteral.Field;

public interface ActionScriptVisitor {
	public void visitArg(ASArg arg);

	public void visitArrayAccessExpression(ASArrayAccessExpression expr);

	public void visitArrayLiteral(ASArrayLiteral lit);

	public void visitAssignmentExpression(ASAssignmentExpression expr);

	public void visitBinaryExpression(ASBinaryExpression expr);

	public void visitBlockStatement(ASBlock stmt);

	public void visitBooleanLiteral(ASBooleanLiteral lit);

	public void visitBreakStatement(ASBreakStatement stmt);

	public void visitCatchClause(ASCatchClause catchClause);

	public void visitClassType(ASClassType type);

	public void visitCompilationUnit(ASCompilationUnit unit);

	public void visitConditionalExpression(ASConditionalExpression expr);

	public void visitContinueStatement(ASContinueStatement stmt);

	public void visitDeclarationStatement(ASDeclarationStatement stmt);

	public void visitDefaultXMLNamespaceStatement(ASDefaultXMLNamespaceStatement stmt);

	public void visitDoWhileStatement(ASDoWhileStatement stmt);

	public void visitDescendantExpression(ASDescendantExpression expr);

	public void visitExpressionAttribute(ASExpressionAttribute expr);

	public void visitExpressionStatement(ASExpressionStatement stmt);

	public void visitField(ASField field);

	public void visitFieldAccessExpression(ASFieldAccessExpression expr);

	public void visitFilterExpression(ASFilterExpression expr);

	public void visitFinallyClause(ASFinallyClause fin);

	public void visitForEachInStatement(ASForEachInStatement stmt);

	public void visitForInStatement(ASForInStatement stmt);

	public void visitForStatement(ASForStatement stmt);

	public void visitFunctionExpression(ASFunctionExpression e);

	public void visitIfStatement(ASIfStatement stmt);

	public void visitIntegerLiteral(ASIntegerLiteral lit);

	public void visitInterfaceType(ASInterfaceType type);

	public void visitInvocationExpression(ASInvocationExpression expr);

	public void visitMetaTag(ASMetaTag tag);

	public void visitMethod(ASMethod method);

	public void visitNewExpression(ASNewExpression expr);

	public void visitNullLiteral(ASNullLiteral lit);

	public void visitObjectField(ASObjectLiteral.Field field);

	public void visitObjectLiteral(ASObjectLiteral lit);

	public void visitPackage(ASPackage pkg);

	public void visitPostfixExpression(ASPostfixExpression expr);

	public void visitPrefixExpression(ASPrefixExpression expr);

	public void visitPropertyAttribute(ASPropertyAttribute expr);

	public void visitRegexpLiteral(ASRegexpLiteral e);

	public void visitReturnStatement(ASReturnStatement stmt);

	public void visitSimpleNameExpression(ASSimpleNameExpression e);

	public void visitStringLiteral(ASStringLiteral lit);

	public void visitStarAttribute(ASStarAttribute expr);

	public void visitSuperStatement(ASSuperStatement stmt);

	public void visitSwitchCase(ASSwitchCase lab);

	public void visitSwitchDefault(ASSwitchDefault lab);

	public void visitSwitchStatement(ASSwitchStatement stmt);

	public void visitThrowStatement(ASThrowStatement stmt);

	public void visitTryStatement(ASTryStatement stmt);

	public void visitUndefinedLiteral(ASUndefinedLiteral lit);

	public void visitVarDeclarationFragment(ASVarDeclarationFragment var);

	public void visitWhileStatement(ASWhileStatement stmt);

	public void visitWithStatement(ASWithStatement stmt);

	public void visitXMLLiteral(ASXMLLiteral lit);
	
	public static class Null implements ActionScriptVisitor {
		public void visitArg(ASArg arg) {
		}
		public void visitArrayAccessExpression(ASArrayAccessExpression expr) {
		}
		public void visitArrayLiteral(ASArrayLiteral lit) {
		}
		public void visitAssignmentExpression(ASAssignmentExpression expr) {
		}
		public void visitBinaryExpression(ASBinaryExpression expr) {
		}
		public void visitBlockStatement(ASBlock stmt) {
		}
		public void visitBooleanLiteral(ASBooleanLiteral lit) {
		}
		public void visitBreakStatement(ASBreakStatement stmt) {
		}
		public void visitCatchClause(ASCatchClause catchClause) {
		}
		public void visitClassType(ASClassType type) {
		}
		public void visitCompilationUnit(ASCompilationUnit unit) {
		}
		public void visitConditionalExpression(ASConditionalExpression expr) {
		}
		public void visitContinueStatement(ASContinueStatement stmt) {
		}
		public void visitDeclarationStatement(ASDeclarationStatement stmt) {
		}
		public void visitDefaultXMLNamespaceStatement(ASDefaultXMLNamespaceStatement stmt) {
		}
		public void visitDoWhileStatement(ASDoWhileStatement stmt) {
		}
		public void visitDescendantExpression(ASDescendantExpression expr) {
		}
		public void visitExpressionAttribute(ASExpressionAttribute expr) {
		}
		public void visitExpressionStatement(ASExpressionStatement stmt) {
		}
		public void visitField(ASField field) {
		}
		public void visitFieldAccessExpression(ASFieldAccessExpression expr) {
		}
		public void visitFilterExpression(ASFilterExpression expr) {
		}
		public void visitFinallyClause(ASFinallyClause fin) {
		}
		public void visitForEachInStatement(ASForEachInStatement stmt) {
		}
		public void visitForInStatement(ASForInStatement stmt) {
		}
		public void visitForStatement(ASForStatement stmt) {
		}
		public void visitFunctionExpression(ASFunctionExpression e) {
		}
		public void visitIfStatement(ASIfStatement stmt) {
		}
		public void visitIntegerLiteral(ASIntegerLiteral lit) {
		}
		public void visitInterfaceType(ASInterfaceType type) {
		}
		public void visitInvocationExpression(ASInvocationExpression expr) {
		}
		public void visitMetaTag(ASMetaTag tag) {
		}
		public void visitMethod(ASMethod method) {
		}
		public void visitNewExpression(ASNewExpression expr) {
		}
		public void visitNullLiteral(ASNullLiteral lit) {
		}
		public void visitObjectField(Field field) {
		}
		public void visitObjectLiteral(ASObjectLiteral lit) {
		}
		public void visitPackage(ASPackage pkg) {
		}
		public void visitPostfixExpression(ASPostfixExpression expr) {
		}
		public void visitPrefixExpression(ASPrefixExpression expr) {
		}
		public void visitPropertyAttribute(ASPropertyAttribute expr) {
		}
		public void visitRegexpLiteral(ASRegexpLiteral e) {
		}
		public void visitReturnStatement(ASReturnStatement stmt) {
		}
		public void visitSimpleNameExpression(ASSimpleNameExpression e) {
		}
		public void visitStringLiteral(ASStringLiteral lit) {
		}
		public void visitStarAttribute(ASStarAttribute expr) {
		}
		public void visitSuperStatement(ASSuperStatement stmt) {
		}
		public void visitSwitchCase(ASSwitchCase lab) {
		}
		public void visitSwitchDefault(ASSwitchDefault lab) {
		}
		public void visitSwitchStatement(ASSwitchStatement stmt) {
		}
		public void visitThrowStatement(ASThrowStatement stmt) {
		}
		public void visitTryStatement(ASTryStatement stmt) {
		}
		public void visitUndefinedLiteral(ASUndefinedLiteral lit) {
		}
		public void visitVarDeclarationFragment(ASVarDeclarationFragment var) {
		}
		public void visitWhileStatement(ASWhileStatement stmt) {
		}
		public void visitWithStatement(ASWithStatement stmt) {
		}
		public void visitXMLLiteral(ASXMLLiteral lit) {
		}
	}
}
