using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Occult.Asteroid_Observations
{
	public class Search_FreeText : Form
	{
		private IContainer components;

		private Label label1;

		private TextBox txtSearchString;

		private Button cmdSearch;

		private ListBox lstResults;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem copyAllToolStripMenuItem;

		private Label label5;

		public Search_FreeText()
		{
			InitializeComponent();
		}

		private void cmdSearch_Click(object sender, EventArgs e)
		{
			GetResults();
		}

		private void GetResults()
		{
			List<string> Events;
			int num = Data_and_Plots.Historical_AllEvents.SearchFreeText(((Control)txtSearchString).get_Text(), out Events);
			lstResults.get_Items().Clear();
			lstResults.get_Items().Add((object)("   " + num + " observer lines contain '" + ((Control)txtSearchString).get_Text() + "'"));
			lstResults.get_Items().Add((object)"");
			for (int i = 0; i < Events.Count; i++)
			{
				lstResults.get_Items().Add((object)Events[i]);
			}
		}

		private void Search_FreeText_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 400)
			{
				((Control)this).set_Width(400);
			}
			if (((Control)this).get_Height() < 200)
			{
				((Control)this).set_Height(200);
			}
			((Control)lstResults).set_Width(((Control)this).get_Width() - 29);
			((Control)lstResults).set_Height(((Control)this).get_Height() - 107);
		}

		private void copySelected_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: true));
		}

		private void copyAll_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(NoPlusMinus: false, SelectedLinesOnly: false));
		}

		private string CollectEvents(bool NoPlusMinus, bool SelectedLinesOnly)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstResults.get_Items().get_Count();
			if (SelectedLinesOnly)
			{
				for (int i = 0; i < lstResults.get_SelectedItems().get_Count(); i++)
				{
					string value = lstResults.get_SelectedItems().get_Item(i).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value);
				}
			}
			else
			{
				for (int j = 0; j < count; j++)
				{
					string value2 = lstResults.get_Items().get_Item(j).ToString()!.Replace("±", " ");
					stringBuilder.AppendLine(value2);
				}
			}
			return stringBuilder.ToString();
		}

		private void lstResults_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			try
			{
				string text = lstResults.get_SelectedItem().ToString();
				if (text.Trim().Length < 10)
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				if ((text.Substring(0, 1) == "#") | (((ListControl)lstResults).get_SelectedIndex() == 0))
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				string asteroidNumber = text.Substring(0, 7);
				string asteroidName = text.Substring(8, 16).Trim();
				int.TryParse(text.Substring(26, 4), out var result);
				int month = 1;
				string text2 = text.Substring(31, 3);
				for (int i = 1; i < 13; i++)
				{
					if (Utilities.ShortMonths[i] == text2)
					{
						month = i;
					}
				}
				int.TryParse(text.Substring(35, 2), out var result2);
				Data_and_Plots.ShowEditor();
				Data_and_Plots.Observations_Editor.DisplayEvent_ByDate_Number(result, month, result2, asteroidNumber, asteroidName);
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

		private void txtSearchString_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 13)
			{
				GetResults();
			}
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
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			label1 = new Label();
			txtSearchString = new TextBox();
			cmdSearch = new Button();
			lstResults = new ListBox();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copyAllToolStripMenuItem = new ToolStripMenuItem();
			label5 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(7, 33));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(334, 17));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("List events where the Free text field contains");
			((Control)txtSearchString).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSearchString).set_Location(new Point(343, 30));
			((Control)txtSearchString).set_Name("txtSearchString");
			((Control)txtSearchString).set_Size(new Size(136, 23));
			((Control)txtSearchString).set_TabIndex(2);
			((Control)txtSearchString).add_KeyDown(new KeyEventHandler(txtSearchString_KeyDown));
			((Control)cmdSearch).set_BackColor(Color.Honeydew);
			((Control)cmdSearch).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSearch).set_Location(new Point(493, 27));
			((Control)cmdSearch).set_Name("cmdSearch");
			((Control)cmdSearch).set_Size(new Size(67, 27));
			((Control)cmdSearch).set_TabIndex(3);
			((Control)cmdSearch).set_Text("Search");
			((ButtonBase)cmdSearch).set_UseVisualStyleBackColor(false);
			((Control)cmdSearch).add_Click((EventHandler)cmdSearch_Click);
			((Control)lstResults).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstResults).set_FormattingEnabled(true);
			lstResults.set_HorizontalScrollbar(true);
			lstResults.set_ItemHeight(14);
			((Control)lstResults).set_Location(new Point(7, 57));
			((Control)lstResults).set_Name("lstResults");
			((Control)lstResults).set_Size(new Size(787, 382));
			((Control)lstResults).set_TabIndex(4);
			((Control)lstResults).add_MouseDown(new MouseEventHandler(lstResults_MouseDown));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)withListToolStripMenuItem });
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(800, 24));
			((Control)menuStrip1).set_TabIndex(5);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copyAllToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List....");
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(148, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copySelected_Click);
			((ToolStripItem)copyAllToolStripMenuItem).set_Name("copyAllToolStripMenuItem");
			((ToolStripItem)copyAllToolStripMenuItem).set_Size(new Size(148, 22));
			((ToolStripItem)copyAllToolStripMenuItem).set_Text("Copy All");
			((ToolStripItem)copyAllToolStripMenuItem).add_Click((EventHandler)copyAll_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(Color.MistyRose);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(571, 35));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(222, 13));
			((Control)label5).set_TabIndex(18);
			((Control)label5).set_Text("Right-click on line to display the event");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(800, 450));
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)lstResults);
			((Control)this).get_Controls().Add((Control)(object)cmdSearch);
			((Control)this).get_Controls().Add((Control)(object)txtSearchString);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("Search_FreeText");
			((Control)this).set_Text("Search Asteroid Observations - free text field");
			((Control)this).add_Resize((EventHandler)Search_FreeText_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
