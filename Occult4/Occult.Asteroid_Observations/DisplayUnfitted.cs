using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class DisplayUnfitted : Form
	{
		private IContainer components;

		private Label label5;

		internal ListBox lstUnfitted;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withRingsToolStripMenuItem;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem copyAllToolStripMenuItem;

		public DisplayUnfitted()
		{
			InitializeComponent();
		}

		private void lstUnfitted_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			try
			{
				string text = lstUnfitted.get_SelectedItem().ToString();
				if (text.Trim().Length < 10)
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				if (text.Substring(0, 3) == "Ast")
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				string asteroidNumber = text.Substring(13, 7);
				string asteroidName = text.PadRight(40).Substring(21, 16).Trim();
				int.TryParse(text.Substring(0, 4), out var result);
				int.TryParse(text.Substring(5, 2), out var result2);
				int.TryParse(text.Substring(8, 2), out var result3);
				Data_and_Plots.ShowEditor();
				Data_and_Plots.Observations_Editor.DisplayEvent_ByDate_Number(result, result2, result3, asteroidNumber, asteroidName);
			}
			catch
			{
			}
		}

		private void ErrorMessage(int left, int top)
		{
			TimedMessageDisplay timedMessageDisplay = new TimedMessageDisplay();
			((Control)timedMessageDisplay).set_Text("Wrong line selected");
			((Control)timedMessageDisplay.txtMessage).set_Text("You must select the line that identifies the event");
			((Control)timedMessageDisplay).Show();
			((Control)timedMessageDisplay).set_Left(left + 10);
			((Control)timedMessageDisplay).set_Top(top + 10);
		}

		private void DisplayUnfitted_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(358);
			if (((Control)this).get_Height() < 100)
			{
				((Control)this).set_Height(120);
			}
			((Control)lstUnfitted).set_Height(((Control)this).get_Height() - 74);
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: true));
		}

		private void copyAllToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private string CollectEvents(bool NoPlusMinus, bool SelectedLinesOnly)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstUnfitted.get_Items().get_Count();
			if (SelectedLinesOnly)
			{
				for (int i = 0; i < lstUnfitted.get_SelectedItems().get_Count(); i++)
				{
					string value = lstUnfitted.get_SelectedItems().get_Item(i).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value);
				}
			}
			else
			{
				for (int j = 0; j < count; j++)
				{
					string value2 = lstUnfitted.get_Items().get_Item(j).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value2);
				}
			}
			return stringBuilder.ToString();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_018c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0196: Expected O, but got Unknown
			label5 = new Label();
			lstUnfitted = new ListBox();
			menuStrip1 = new MenuStrip();
			withRingsToolStripMenuItem = new ToolStripMenuItem();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyAllToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(Color.MistyRose);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(115, 3));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(222, 13));
			((Control)label5).set_TabIndex(20);
			((Control)label5).set_Text("Right-click on line to display the event");
			((Control)lstUnfitted).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstUnfitted).set_FormattingEnabled(true);
			lstUnfitted.set_ItemHeight(14);
			((Control)lstUnfitted).set_Location(new Point(1, 27));
			((Control)lstUnfitted).set_Name("lstUnfitted");
			lstUnfitted.set_SelectionMode((SelectionMode)3);
			((Control)lstUnfitted).set_Size(new Size(337, 340));
			((Control)lstUnfitted).set_TabIndex(19);
			((Control)lstUnfitted).add_MouseDown(new MouseEventHandler(lstUnfitted_MouseDown));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)withRingsToolStripMenuItem });
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(341, 24));
			((Control)menuStrip1).set_TabIndex(21);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withRingsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)copyAllToolStripMenuItem
			});
			((ToolStripItem)withRingsToolStripMenuItem).set_Name("withRingsToolStripMenuItem");
			((ToolStripItem)withRingsToolStripMenuItem).set_Size(new Size(88, 20));
			((ToolStripItem)withRingsToolStripMenuItem).set_Text("with events...");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyAllToolStripMenuItem).set_Name("copyAllToolStripMenuItem");
			((ToolStripItem)copyAllToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)copyAllToolStripMenuItem).set_Text("copy All");
			((ToolStripItem)copyAllToolStripMenuItem).add_Click((EventHandler)copyAllToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(341, 375));
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)lstUnfitted);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).set_Name("DisplayUnfitted");
			((Control)this).set_Text("Display Unfitted events");
			((Control)this).add_Resize((EventHandler)DisplayUnfitted_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
