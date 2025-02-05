use attribute_derive::FromAttr;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{Data, Fields};

#[derive(FromAttr, PartialEq, Debug)]
#[attribute(ident = code)]
struct Code(u8);

#[proc_macro_derive(Instruction, attributes(code))]
pub fn instruction(item: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(item as syn::DeriveInput);

    match &input.data {
        Data::Enum(e) => {
            let ident = &input.ident;
            // fuck, shit code
            let ((variant_any_pat, read_fields), (variant, code)) = e
                .variants
                .iter()
                .enumerate()
                .map(|(idx, variant)| {
                    /* let code = Code::from_attributes(&variant.attrs)
                    .expect("missing or invalid code attribute"); */
                    let is_struct = matches!(variant.fields, Fields::Named(_));
                    let is_tuple = matches!(variant.fields, Fields::Unnamed(_));
                    let variant_id = &variant.ident;
                    if is_struct {
                        (
                            (quote!(#ident :: #variant_id { .. }), {
                                let fields = variant
                                    .fields
                                    .iter()
                                    .map(|id| id.ident.as_ref().unwrap())
                                    .collect::<Vec<_>>();
                                quote! {
                                    {
                                        #(
                                            #fields: ReadFromBytes::read_from_bytes(reader)?,
                                        )*
                                    }
                                }
                            }),
                            (quote!(#ident :: #variant_id), idx as u8),
                        )
                    } else if is_tuple {
                        (
                            (quote!(#ident :: #variant_id (..)), {
                                let fields =
                                    variant.fields.iter().map(|_| quote!()).collect::<Vec<_>>();
                                quote! {
                                    (
                                        #(
                                            ReadFromBytes::read_from_bytes(reader)? #fields,
                                        )*
                                    )
                                }
                            }),
                            (quote!(#ident :: #variant_id), idx as u8),
                        )
                    } else {
                        // is unit
                        (
                            (quote!(#ident :: #variant_id), quote!()),
                            (quote!(#ident :: #variant_id), idx as u8),
                        )
                    }
                })
                .collect::<((Vec<_>, Vec<_>), (Vec<_>, Vec<_>))>();

            let write_field = e
                .variants
                .iter()
                .enumerate()
                .map(|(_, variant)| {
                    let is_struct = matches!(variant.fields, Fields::Named(_));
                    let is_tuple = matches!(variant.fields, Fields::Unnamed(_));
                    let variant_id = &variant.ident;
                    let fields = variant
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(idx, _)| format_ident!("_{}", idx))
                        .collect::<Vec<_>>();

                    if is_struct {
                        quote! {
                            #ident :: #variant_id { #(#fields)*, } => {
                                #(#fields.write_to_bytes(writer)?;)*
                            }
                        }
                    } else if is_tuple {
                        quote! {
                            #ident :: #variant_id ( #(#fields)*, ) => {
                                #(#fields.write_to_bytes(writer)?;)*
                            }
                        }
                    } else {
                        quote!(#ident :: #variant_id => {})
                    }
                })
                .collect::<Vec<_>>();

            (quote! {
                impl #ident {
                    pub fn code(&self) -> u8 {
                        match self {
                            #(
                                #variant_any_pat => #code,
                            )*
                        }
                    }
                }

                impl ReadFromBytes for #ident {
                    fn read_from_bytes<R>(reader: &mut R) -> std::io::Result<Self> where R: byteorder::ReadBytesExt {
                        let code = reader.read_u8()?;
                        match code {
                            #(
                                #code => Ok(#variant #read_fields),
                            )*
                            _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, "invalid opcode")),
                        }
                    }
                }

                impl WriteToBytes for #ident {
                    fn write_to_bytes<W>(&self, writer: &mut W) -> std::io::Result<()> where W: byteorder::WriteBytesExt {
                        writer.write_u8(self.code())?;
                        match self {
                            #(
                                #write_field
                            )*
                        }
                        Ok(())
                    }
                }
            })
            .into()
        }
        _ => {
            panic!("`OpCode` can only be applied to enums.");
        }
    }
}
