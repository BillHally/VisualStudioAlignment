#nullable enable

using System;
using System.Collections.Generic;
using System.Linq;

using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;

namespace Hally.Alignment.VisualStudio
{
    /// <summary>
    ///  Some of these helpers are from https://github.com/dibarbet/roslyn/blob/main/src/EditorFeatures/Core/Shared/Extensions/ITextViewExtensions.cs
    /// </summary>
    public static class TextViewExtensions
    {
        public static NormalizedSnapshotSpanCollection GetSpanInView(this ITextView textView, SnapshotSpan span)
        {
            return textView.BufferGraph.MapUpToSnapshot(span, SpanTrackingMode.EdgeInclusive, textView.TextSnapshot);
        }

        public static void SetBoxSelection(this ITextView textView, IEnumerable<SnapshotSpan> spans, bool isReversed)
        {
            var first = textView.GetSpanInView(spans.First()).Single();
            var last  = textView.GetSpanInView(spans.Last()).Single();

            var length = last.End - first.Start;

            textView.GetMultiSelectionBroker().SetBoxSelection(new Selection(new SnapshotSpan(textView.TextSnapshot, first.Start, length), isReversed));
        }
    }

    public record SelectedLines(string[] Lines, int[] LineIndices, bool IsReversed)
    {
        public static SelectedLines Empty => new SelectedLines(Array.Empty<string>(), Array.Empty<int>(), false);

        public int Length => Lines.Length;

        public override string ToString()
        {
            return String.Join(", ", LineIndices);
        }
    }

    public static class AlignLines
    {
        public static SelectedLines GetSelectedLines(ILogger logger, ITextView textView, IEditorOperations editorOperations)
        {
            var selection = textView.Selection;

            logger.Debug($"Selection.IsActive  : {selection.IsActive}\n");
            logger.Debug($"Selection.IsEmpty   : {selection.IsEmpty}\n");
            logger.Debug($"Selection.Mode = Box: {selection.Mode == TextSelectionMode.Box}\n");

            if (!selection.IsActive)
            {
                // TODO: check whether this test is necessary (i.e. if false, is this method ever invoked?)
                return SelectedLines.Empty;
            }
            else if (selection.IsEmpty)
            {
                // 1. There's no selection, so, first of all, get the current line
                var textSnapshot = textView.TextSnapshot;
                var initialLineNumber = textSnapshot.GetLineNumberFromPosition(textView.Caret.Position.BufferPosition);
                var initialLine = textSnapshot.GetLineFromLineNumber(initialLineNumber).GetText();

                // 2. Find lines immediately above the current one which "match" the current line
                var currentLineNumber = initialLineNumber;

                for (
                    var i = initialLineNumber - 1;
                    i >= 0 &&
                    Alignment.LinesMatch(initialLine, textSnapshot.GetLineFromLineNumber(i).GetText());
                    --i
                ) {
                    currentLineNumber = i;
                }
                var firstLineIndex = currentLineNumber;

                // 3. Find lines immediately below the current one which "match" the current line
                currentLineNumber = initialLineNumber; // Reset currentLineNumber
                for (
                    var i = initialLineNumber + 1;
                    i < textSnapshot.LineCount &&
                    Alignment.LinesMatch(initialLine, textSnapshot.GetLineFromLineNumber(i).GetText());
                    ++i
                ) {
                    currentLineNumber = i;
                }

                var lastLineIndex = currentLineNumber;

                // 4. Get all the relevant lines and return them
                var lines = new string[lastLineIndex - firstLineIndex + 1]; // Note: despite this declaring non-nullable strings, it's initialized with nulls!
                var lineIndices = new int[lines.Length];

                for (var i = 0; i < lines.Length; ++i)
                {
                    var lineIndex = firstLineIndex + i;
                    var line = textSnapshot.GetLineFromLineNumber(lineIndex);
                    lines[i]   = line.GetTextIncludingLineBreak().Trim('\n');
                    lineIndices[i] = lineIndex;
                }

                return new SelectedLines(lines, lineIndices, IsReversed: false);
            }
            else if (selection.Mode == TextSelectionMode.Box)
            {
                // It's a "Box" selection i.e. there are multiple selection spans - 1 per line
                var lines = new string[selection.SelectedSpans.Count];

                foreach (var i in Enumerable.Range(0, selection.SelectedSpans.Count))
                {
                    var selectedSpan = selection.SelectedSpans[i];
                    lines[i] = selectedSpan.GetText();
                }

                return new SelectedLines(lines, Array.Empty<int>(), IsReversed: selection.IsReversed);
            }
            else
            {
                // It's a "stream" selection i.e. a single continuous selection, so there's only a single span selected
                var selectedSpan = selection.SelectedSpans[0];
                var start = selectedSpan.Span.Start;

                // If necessary, extend the selection to the start of the first line of the selection, so that columns
                // of the all the lines in the selection match up
                var textLine = selection.Start.Position.GetContainingLine();
                if (textLine.Start.Position != start)
                {
                    selectedSpan =
                        new SnapshotSpan(
                            selectedSpan.Snapshot,
                            textLine.Start.Position,
                            start - textLine.Start.Position + selectedSpan.Length
                        );

                    textView.Selection.Select(selectedSpan, selection.IsReversed);
                }

                var text = selectedSpan.GetText();
                var lines = text.Split('\n');
                return new SelectedLines(lines, Array.Empty<int>(), IsReversed: selection.IsReversed);
            }
        }

        public static void ProcessSelectedLines(ILogger logger, ITextView textView, IEditorOperations editorOperations, Func<string[], string[]> align)
        {
            try
            {
                var lines = GetSelectedLines(logger, textView, editorOperations);

                if (lines.Length == 0)
                {
                    // Nothing to do
                    return;
                }
                else
                {
                    var selection    = textView.Selection;
                    var updatedLines = align.Invoke(lines.Lines);

                    if (selection.Mode == TextSelectionMode.Box)
                    {
                        // Keep a copy - it's expensive to recalculate when selection changes, and we only want the original values
                        var selectedSpans = selection.SelectedSpans.ToArray();
                        var updatedSnapshotSpans = new SnapshotSpan[lines.Length];

                        // Iterate in reverse, because the start and end points of each span will be affected by any changes
                        // to the length of spans located before it in the text.
                        foreach (var i in Enumerable.Range(0, lines.Length).Reverse())
                        {
                            updatedSnapshotSpans[i] =
                                new SnapshotSpan(
                                    textView.TextBuffer.Replace(selectedSpans[i], updatedLines[i]),
                                    selectedSpans[i].Span.Start,
                                    updatedLines[i].Length
                                );
                        }

                        // Reselect the original selection, expanded or reduced to the extent of the updated text
                        textView.SetBoxSelection(updatedSnapshotSpans, lines.IsReversed);
                    }
                    else if (selection.IsEmpty)
                    {
                        // Remember the original caret position
                        var originalPosition = textView.Caret.Position.BufferPosition;
                        var originalLine = originalPosition.GetContainingLine();
                        var originalOffsetFromStartOfLine = originalPosition - originalLine.Start.Position;

                        // Get the span to replace
                        var firstLineToReplace = lines.LineIndices[0];
                        var lastLineToReplace  = lines.LineIndices[lines.LineIndices.Length - 1];

                        var startOfFirstLineToReplace = textView.TextSnapshot.GetLineFromLineNumber(firstLineToReplace).Start;
                        var endOfLastLineToReplace    = textView.TextSnapshot.GetLineFromLineNumber(lastLineToReplace).End;

                        var spanToReplace = new Span(startOfFirstLineToReplace, endOfLastLineToReplace - startOfFirstLineToReplace + 1);

                        // Update the text
                        var updatedText = String.Join("\n", updatedLines);
                        textView.TextBuffer.Replace(spanToReplace, updatedText);

                        // Reset the caret position (approximately) TODO: do this accurately
                        var updatedLine = textView.TextSnapshot.GetLineFromLineNumber(originalLine.LineNumber);
                        var updatedPosition = updatedLine.Start.Position + originalOffsetFromStartOfLine;
                        textView.Caret.MoveTo(new SnapshotPoint(textView.TextSnapshot, updatedPosition));
                    }
                    else
                    {
                        // Update the text
                        var updatedText = String.Join("\n", updatedLines);
                        var selectedSpan = selection.SelectedSpans[0];
                        var updatedSnapshot = textView.TextBuffer.Replace(selectedSpan, updatedText);

                        // Reset the selection, expanded or reduced to the extent of the updated text
                        var updatedSnapshotSpan = new SnapshotSpan(updatedSnapshot, selectedSpan.Span.Start, updatedText.Length);
                        textView.Selection.Select(updatedSnapshotSpan, lines.IsReversed);

                        // If the original selection had the caret at the start, move it back there now (the text replacement
                        // seems to always leave the caret at the end, and redoing the selection doesn't affect the caret).
                        if (lines.IsReversed)
                        {
                            textView.Caret.MoveTo(selectedSpan.Start);
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                logger.Debug($"ERROR: {ex.Message}\n\n{ex}");
            }
        }
    }
}
