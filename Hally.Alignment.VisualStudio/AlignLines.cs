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

    public record SelectedLines(string[] Lines, bool IsReversed)
    {
        public static SelectedLines Empty => new SelectedLines(Array.Empty<string>(), false);

        public int Length => Lines.Length;
    }

    public static class AlignLines
    {
        public static SelectedLines GetSelectedLines(ITextView textView, IEditorOperations editorOperations)
        {
            var selection = textView.Selection;

            if (!selection.IsActive)
            {
                // TODO: check whether this test is necessary (i.e. if false, is this method ever invoked?)
                return SelectedLines.Empty;
            }

            if (selection.IsEmpty)
            {
                // TODO: do something sensible when there's no selection
                return SelectedLines.Empty;
            }

            if (selection.Mode == TextSelectionMode.Box)
            {
                // It's a "Box" selection i.e. there are multiple selection spans - 1 per line
                var lines = new string[selection.SelectedSpans.Count];

                foreach (var i in Enumerable.Range(0, selection.SelectedSpans.Count))
                {
                    var selectedSpan = selection.SelectedSpans[i];
                    lines[i] = selectedSpan.GetText();
                }

                return new SelectedLines(lines, selection.IsReversed);
            }
            else
            {
                // It's a "stream" selection i.e. a single continuous selection, so there's only a single span selected
                var selectedSpan = selection.SelectedSpans[0];
                var start = selectedSpan.Span.Start;

                var isReversed = selection.IsReversed;

                // If necessary, extend the selection to the start of the first line of the selection, so that columns
                // of the all the lines in the selection match up
                var textLine = selection.Start.Position.GetContainingLine();
                if (textLine.Start.Position != start)
                {
                    selectedSpan = new SnapshotSpan(selectedSpan.Snapshot, textLine.Start.Position, start - textLine.Start.Position + selectedSpan.Length);
                    textView.Selection.Select(selectedSpan, isReversed);
                }

                var text = selectedSpan.GetText();
                var lines = text.Split('\n');

                return new SelectedLines(lines, isReversed);
            }
        }

        public static void ProcessSelectedLines(ITextView textView, IEditorOperations editorOperations, Func<string[], string[]> align)
        {
#if DEBUG
            var logs = new List<string>();
            try
            {
#endif
                var lines = GetSelectedLines(textView, editorOperations);

                if (lines.Length == 0)
                {
                    return;
                }
                else
                {
                    var selection    = textView.Selection;
                    var isReversed   = selection.IsReversed;
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
                            var selectedSpan = selectedSpans[i];
                            var start = selectedSpan.Span.Start;
                            var updatedText = updatedLines[i];

                            var updatedSnapshot = textView.TextBuffer.Replace(selectedSpan, updatedLines[i]);
                            updatedSnapshotSpans[i] = new SnapshotSpan(updatedSnapshot, new Span(start, updatedText.Length));
                        }

                        // Reselect the original selection, expanded or reduced to the extent of the updated text
                        textView.SetBoxSelection(updatedSnapshotSpans, isReversed);
                    }
                    else
                    {
                        var selectedSpan = selection.SelectedSpans[0];
                        var start = selectedSpan.Span.Start;
                        var updatedText = String.Join("\n", updatedLines);

                        var updatedSnapshot = textView.TextBuffer.Replace(selectedSpan, updatedText);

                        // Redo the original selection, expanded or reduced to the extent of the updated text
                        var updatedSnapshotSpan = new SnapshotSpan(updatedSnapshot, start, updatedText.Length);
                        textView.Selection.Select(updatedSnapshotSpan, isReversed);

                        // If the original selection had the caret at the start, move it back there now (the text replacement leaves the caret at the end,
                        // and redoing the selection doesn't affect the caret).
                        if (isReversed)
                        {
                            textView.Caret.MoveTo(selection.SelectedSpans[0].Start);
                        }
                    }
                }
#if DEBUG
            }
            catch (Exception ex)
            {
                var log = String.Join("\n", logs);
                textView.TextBuffer.Insert(textView.Caret.Position.BufferPosition, log + "\n" + ex.ToString());
            }
#endif
        }
    }
}
