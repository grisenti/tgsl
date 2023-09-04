use crate::compiler::ast::expression::expr::Dot;
use crate::compiler::ast::parsed_type::ParsedFunctionType;
use crate::compiler::ast::statement::stmt::ModuleDecl;
use crate::compiler::ast::visitor::ParsedTypeVisitor;
use crate::compiler::ast::{ExprHandle, StmtHandle, TypeHandle};
use json::object;
use json::JsonValue;

use crate::compiler::identifier::{FunctionId, VariableIdentifier};
use crate::compiler::identifier::{Identifier, StructId};
use crate::compiler::lexer::Token;

use super::expression::*;
use super::statement::*;
use super::visitor::ExprVisitor;
use super::visitor::ProgramVisitor;
use super::visitor::StmtVisitor;
use super::AST;

impl From<Identifier> for JsonValue {
  fn from(value: Identifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<VariableIdentifier> for JsonValue {
  fn from(value: VariableIdentifier) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<StructId> for JsonValue {
  fn from(value: StructId) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<FunctionId> for JsonValue {
  fn from(value: FunctionId) -> Self {
    JsonValue::String(format!("{value:?}"))
  }
}

impl From<Token<'_>> for JsonValue {
  fn from(value: Token) -> Self {
    JsonValue::String(format!("{:?}", value))
  }
}

fn expr_list_to_json(expressions: &[ExprHandle], ast: &AST) -> JsonValue {
  let mut printer = ASTJSONPrinter {};
  JsonValue::Array(
    expressions
      .iter()
      .map(|&e| printer.visit_expr(ast, e))
      .collect::<Vec<_>>(),
  )
}

fn stmt_list_to_json(statements: &[StmtHandle], ast: &AST) -> JsonValue {
  let mut printer = ASTJSONPrinter {};
  JsonValue::Array(
    statements
      .iter()
      .map(|&s| printer.visit_stmt(ast, s))
      .collect::<Vec<_>>(),
  )
}

fn parsed_type_list_to_json(parsed_types: &[TypeHandle], ast: &AST) -> JsonValue {
  let mut printer = ASTJSONPrinter {};
  JsonValue::Array(
    parsed_types
      .iter()
      .map(|&t| printer.visit_parsed_type(ast, t))
      .collect::<Vec<_>>(),
  )
}

pub struct ASTJSONPrinter {}

impl ExprVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_literal(&mut self, ast: &AST, literal: &expr::Literal) -> JsonValue {
    object! {
      "Literal": {
        "value": literal.value
      }
    }
  }

  fn visit_id(&mut self, _ast: &AST, id: &expr::Id) -> JsonValue {
    object! {
      "Id": {
        "id": id.id
      }
    }
  }

  fn visit_paren(&mut self, ast: &AST, paren: &expr::Paren) -> JsonValue {
    object! {
      "Paren": {
        "expr": self.visit_expr(ast, paren.inner)
      }
    }
  }

  fn visit_assignment(&mut self, ast: &AST, assignment: &expr::Assignment) -> JsonValue {
    object! {
      "Assignment": {
        lhs: self.visit_expr(ast, assignment.lhs),
        rhs: self.visit_expr(ast, assignment.rhs),
      }
    }
  }

  fn visit_binary(&mut self, ast: &AST, binary: &expr::Binary) -> JsonValue {
    object! {
      "Binary": {
        left: self.visit_expr(ast, binary.left),
        operator: binary.operator,
        right: self.visit_expr(ast, binary.right)
      }
    }
  }

  fn visit_unary(&mut self, ast: &AST, unary: &expr::Unary) -> JsonValue {
    object! {
      "Unary": {
        "operator": unary.operator,
        "right": self.visit_expr(ast, unary.right),
      }
    }
  }

  fn visit_lambda(&mut self, ast: &AST, lambda: &expr::Lambda) -> JsonValue {
    object! {
      "Lambda": {
        "parameter_names": lambda.parameter_names.clone(),
        "parameter_types": parsed_type_list_to_json(&lambda.parameter_types, ast),
        "return_type": self.visit_parsed_type(ast, lambda.return_type),
        "body": stmt_list_to_json(&lambda.body, ast)
      }
    }
  }

  fn visit_fn_call(&mut self, ast: &AST, fn_call: &expr::FnCall) -> JsonValue {
    object! {
      "FnCall": {
        "func": self.visit_expr(ast, fn_call.func),
        "arguments": expr_list_to_json(&fn_call.arguments, ast)
      }
    }
  }

  fn visit_dot(&mut self, ast: &AST, dot: &Dot) -> JsonValue {
    object! {
      "Dot": {
        lhs: self.visit_expr(ast, dot.lhs),
        rhs: dot.rhs
      }
    }
  }

  fn visit_constructor(&mut self, ast: &AST, constructor: &expr::Construct) -> JsonValue {
    object! {
      "Constructor": {
        "type_name": constructor.type_name,
        "arguments": expr_list_to_json(&constructor.arguments, ast)
      }
    }
  }
}

impl StmtVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_var_decl(&mut self, ast: &AST, var_decl: &stmt::VarDecl) -> JsonValue {
    object! {
      "VarDecl": {
        "name": var_decl.name,
        "specified_type": self.visit_parsed_type(ast, var_decl.specified_type),
        "init_expr": self.visit_expr(ast, var_decl.init_expr)
      }
    }
  }

  fn visit_stmt_expr(&mut self, ast: &AST, expr: &stmt::StmtExpr) -> JsonValue {
    object! {
      "StmtExpr": {
        "expr": self.visit_expr(ast, expr.expr),
      }
    }
  }

  fn visit_block(&mut self, ast: &AST, block: &stmt::Block) -> JsonValue {
    object! {
      "Block": {
        "statements": stmt_list_to_json(&block.statements, ast)
      }
    }
  }

  fn visit_if_branch(&mut self, ast: &AST, if_branch: &stmt::IfBranch) -> JsonValue {
    let else_branch = if let Some(stmt) = if_branch.else_branch {
      self.visit_stmt(ast, stmt)
    } else {
      JsonValue::Null
    };
    object! {
      "IfBranch": {
        "condition": self.visit_expr(ast, if_branch.condition),
        "true branch": self.visit_stmt(ast, if_branch.true_branch),
        "else branch": else_branch
      }
    }
  }

  fn visit_while(&mut self, ast: &AST, while_stmt: &stmt::While) -> JsonValue {
    object! {
      "While": {
        "condition": self.visit_expr(ast, while_stmt.condition),
        "body": self.visit_stmt(ast, while_stmt.loop_body)
      }
    }
  }

  fn visit_function_definition(
    &mut self,
    ast: &AST,
    function_definition: &stmt::FunctionDefinition,
  ) -> JsonValue {
    object! {
      "FunctionDefinition": {
        "name": function_definition.name,
        "parameter_names": function_definition.parameter_names.clone(),
        "parameter_types": parsed_type_list_to_json(&function_definition.parameter_types, ast),
        "return_type": self.visit_parsed_type(ast, function_definition.return_type),
        "body": stmt_list_to_json(&function_definition.body, ast)
      }
    }
  }

  fn visit_function_declaration(
    &mut self,
    ast: &AST,
    function_declaration: &stmt::FunctionDeclaration,
  ) -> JsonValue {
    object! {
        "FunctionDeclaration": {
          "name": function_declaration.name,
          "parameter_names": function_declaration.parameter_names.clone(),
          "parameter_types": parsed_type_list_to_json(&function_declaration.parameter_types, ast),
          "return_type": self.visit_parsed_type(ast, function_declaration.return_type)
        }
    }
  }

  fn visit_extern_function(
    &mut self,
    ast: &AST,
    extern_function: &stmt::ExternFunction,
  ) -> JsonValue {
    object! {
      "ExternalFunction": {
        "name": extern_function.name,
        "parameter_names": extern_function.parameter_names.clone(),
        "parameter_types": parsed_type_list_to_json(&extern_function.parameter_types, ast),
        "return_type": self.visit_parsed_type(ast, extern_function.return_type)
      }
    }
  }

  fn visit_break(&mut self, _ast: &AST) -> JsonValue {
    JsonValue::String("Break".to_string())
  }

  fn visit_return(&mut self, ast: &AST, return_stmt: &stmt::Return) -> JsonValue {
    let ret_expr = if let Some(expr) = return_stmt.expr {
      self.visit_expr(ast, expr)
    } else {
      JsonValue::Null
    };
    object! {
      "Return": {
        "expr": ret_expr
      }
    }
  }

  fn visit_struct(&mut self, ast: &AST, struct_stmt: &stmt::Struct) -> JsonValue {
    object! {
      "Struct": {
        "name": struct_stmt.name,
        "member_names": struct_stmt.member_names.clone(),
        "member_types": parsed_type_list_to_json(&struct_stmt.member_types, ast)
      }
    }
  }

  fn visit_import(&mut self, _ast: &AST, import: &stmt::Import) -> JsonValue {
    object! {
      "Import": {
        "module_name": import.module_name
      }
    }
  }

  fn visit_module_decl(&mut self, ast: &AST, module_decl: &ModuleDecl) -> JsonValue {
    object! {
      "ModuleDecl": {
        "name": module_decl.name
      }
    }
  }
}

impl ParsedTypeVisitor<JsonValue> for ASTJSONPrinter {
  fn visit_num(&mut self, ast: &AST) -> JsonValue {
    JsonValue::String("num".to_string())
  }

  fn visit_str(&mut self, ast: &AST) -> JsonValue {
    JsonValue::String("str".to_string())
  }

  fn visit_bool(&mut self, ast: &AST) -> JsonValue {
    JsonValue::String("bool".to_string())
  }

  fn visit_nothing(&mut self, ast: &AST) -> JsonValue {
    JsonValue::String("nothing".to_string())
  }

  fn visit_any(&mut self, ast: &AST) -> JsonValue {
    JsonValue::String("any".to_string())
  }

  fn visit_named(&mut self, ast: &AST, name: &str) -> JsonValue {
    object! {named: name}
  }

  fn visit_function(&mut self, ast: &AST, function: &ParsedFunctionType) -> JsonValue {
    object! {
      "Function": {
        "parameters": parsed_type_list_to_json(&function.signature(), ast),
        "return_type": self.visit_parsed_type(ast, function.return_type()),
      }
    }
  }
}

impl ASTJSONPrinter {
  pub fn print_to_string(ast: &AST) -> String {
    let program = ProgramVisitor::new(ASTJSONPrinter {}, ast).collect::<Vec<_>>();
    json::stringify_pretty(program, 1)
  }

  pub fn to_json(ast: &AST) -> JsonValue {
    let program = ProgramVisitor::new(ASTJSONPrinter {}, ast).collect::<Vec<_>>();
    JsonValue::Array(program)
  }
}
