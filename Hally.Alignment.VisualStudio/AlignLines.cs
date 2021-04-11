using System;

using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;

namespace Hally.Alignment.VisualStudio
{
    public static class AlignLines
    {
        public static void ProcessSelectedLines(ITextView textView, IEditorOperations editorOperations, Func<string, string> align)
        {
            var selection = textView.Selection;

            if (!selection.IsActive)
            {
                // TODO: check whether this test is necessary (i.e. if false, is this method ever invoked?)
                return;
            }

            if (selection.IsEmpty)
            {
                // TODO: do something sensible when there's no selection
                return;
            }

            if (selection.Mode == TextSelectionMode.Box)
            {
                // TODO: do something sensible when there's a Box selection
                return;
            }

            var isReversed = selection.IsReversed;
            var selectedSpan = selection.SelectedSpans[0]; // When it's not a Box selection there's only 1 selected span
            var start = selectedSpan.Span.Start;

#if DEBUG
            try
            {
#endif
                // If necessary, extend the selection to the start of the first line of the selection, so that columns
                // of the all the lines in the selection match up
                var textLine = selection.Start.Position.GetContainingLine();
                if (textLine.Start.Position != start)
                {
                    selectedSpan = new SnapshotSpan(selectedSpan.Snapshot, textLine.Start.Position, start - textLine.Start.Position + selectedSpan.Length);
                    start = textLine.Start.Position;
                    textView.Selection.Select(selectedSpan, isReversed);
                }

                // Realign the selected text and replace the current selection with the result
                var text = selectedSpan.GetText();
                var updatedText = align.Invoke(text);
                var updatedSnapshot = textView.TextBuffer.Replace(selectedSpan, updatedText);

                // Redo the original selection, expanded or reduced to the extent of the updated text
                var updatedSnapshotSpan = new SnapshotSpan(updatedSnapshot, start, updatedText.Length);
                textView.Selection.Select(updatedSnapshotSpan, isReversed);

                // If the original selection had the caret at the start, move it back there now (the text replacement leaves the caret at the end,
                // and redoing the selection doesn't affect the caret)
                if (isReversed)
                {
                    textView.Caret.MoveTo(updatedSnapshotSpan.Start);
                }
#if DEBUG
            }
            catch (Exception ex) {
                textView.TextBuffer.Insert(selectedSpan.Span.Start, ex.ToString());
            }
#endif
        }
    }
}
