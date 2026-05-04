-- pfh-docx-styles.lua
-- Applies PFH-compliant paragraph styles during docx rendering:
--   • Bibliography  → hanging indent for references
--   • First Paragraph → no first-line indent after headings

if FORMAT ~= "docx" then
  return {}
end

local in_references = false

-- Remove automatic title block (title, author, date) from docx
-- since we use a custom cover page instead
local function remove_title_meta(meta)
  meta.title = nil
  meta.author = nil
  meta.date = nil
  return meta
end

local function styled_para(block, style)
  return pandoc.Div(
    pandoc.Para(block.content),
    pandoc.Attr("", {}, {["custom-style"] = style})
  )
end

return {
  {
    Meta = remove_title_meta
  },
  {
    Pandoc = function(doc)
      local result    = pandoc.List()
      local prev_is_header = false

      for _, block in ipairs(doc.blocks) do

        if block.t == "Header" then
          local title = pandoc.utils.stringify(block)
          in_references = (title == "References")
          prev_is_header = true
          result:insert(block)

        elseif block.t == "Para" then
          if in_references then
            result:insert(styled_para(block, "Bibliography"))
          elseif prev_is_header then
            result:insert(styled_para(block, "First Paragraph"))
            prev_is_header = false
          else
            prev_is_header = false
            result:insert(block)
          end

        elseif block.t == "Table" or
               block.t == "BulletList" or
               block.t == "OrderedList" or
               block.t == "BlockQuote" then
          -- These reset the "first paragraph" state
          prev_is_header = false
          result:insert(block)

        else
          -- RawBlock, Div, HorizontalRule, etc. do NOT reset state
          -- so a heading followed by a code-chunk output still marks
          -- the next text paragraph as "First Paragraph"
          result:insert(block)
        end

      end

      doc.blocks = result
      return doc
    end
  }
}
