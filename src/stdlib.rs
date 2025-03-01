use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use inkwell::module::Linkage;
use crate::code_generator::CodeGenerator;

pub trait Stdlib<'a> {
    fn get_strcat(&mut self) -> FunctionValue;
}

impl<'a> Stdlib<'a> for CodeGenerator<'a> {
    fn get_strcat(&mut self) -> FunctionValue {
        if let Some(strcat) = self.functions.get("strcat") {
            return *strcat
        }

        let sprintf_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );
        let strcat = self
            .module
            .add_function("strcat", sprintf_type, Some(Linkage::External));

        self.functions.insert("strcat".to_string(), strcat);
        strcat

    }

}

