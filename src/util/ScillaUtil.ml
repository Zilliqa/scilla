module FilePathInfix = struct
  let ( ^/ ) = FilePath.concat
  let ( ^. ) = FilePath.add_extension
end

