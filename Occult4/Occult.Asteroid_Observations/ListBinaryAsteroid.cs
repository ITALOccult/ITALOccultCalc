using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class ListBinaryAsteroid : Form
	{
		private IContainer components;

		private ListBox lstBinaries;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withBinaryAsteroidsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem sortToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNameToolStripMenuItem;

		private ToolStripMenuItem byAsteroidNumberToolStripMenuItem;

		private ToolStripMenuItem byDateToolStripMenuItem;

		private ToolStripMenuItem bySeparationToolStripMenuItem;

		private ToolStripMenuItem byMajorAxisOfCompanionToolStripMenuItem;

		private ToolStripMenuItem copySelectedToolStripMenuItem;

		private ToolStripMenuItem listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem byCBETNumberToolStripMenuItem;

		private Label label5;

		public ListBinaryAsteroid()
		{
			InitializeComponent();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(Selected: false));
		}

		private void copySelectedToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents(Selected: true));
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents(Selected: false));
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents(Selected: false));
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_AsteroidResults = Output.SavePredictionText(CollectEvents(Selected: false), "Binary asteroid occultation results", Settings.Default.Save_AsteroidResults);
		}

		private string CollectEvents(bool Selected)
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstBinaries.get_Items().get_Count();
			if (Selected)
			{
				foreach (object selectedItem in lstBinaries.get_SelectedItems())
				{
					stringBuilder.AppendLine(selectedItem.ToString());
				}
			}
			else
			{
				for (int i = 0; i < count; i++)
				{
					stringBuilder.AppendLine(lstBinaries.get_Items().get_Item(i).ToString());
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void byAsteroidNameToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroid.SortField = 0;
			Asteroid_Observations_Reports.BinaryList.Sort();
			Display();
		}

		private void byAsteroidNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroid.SortField = 1;
			Asteroid_Observations_Reports.BinaryList.Sort();
			Display();
		}

		private void byDateToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroid.SortField = 2;
			Asteroid_Observations_Reports.BinaryList.Sort();
			Display();
		}

		private void bySeparationToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroid.SortField = 3;
			Asteroid_Observations_Reports.BinaryList.Sort();
			Display();
		}

		private void byMajorAxisOfCompanionToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroid.SortField = 4;
			Asteroid_Observations_Reports.BinaryList.Sort();
			Display();
		}

		private void byCBETNumberToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroid.SortField = 5;
			Asteroid_Observations_Reports.BinaryList.Sort();
			Display();
		}

		public void Display()
		{
			int num = 0;
			lstBinaries.get_Items().Clear();
			lstBinaries.get_Items().Add((object)(Asteroid_Observations_Reports.BinaryList.Count + " binary asteroids observed in asteroidal occultations"));
			switch (BinaryAsteroid.SortField)
			{
			case 0:
				lstBinaries.get_Items().Add((object)"Sorted by Name");
				break;
			case 1:
				lstBinaries.get_Items().Add((object)"Sorted by Number");
				break;
			case 2:
				lstBinaries.get_Items().Add((object)"Sorted by Date (most recent first)");
				break;
			case 3:
				lstBinaries.get_Items().Add((object)"Sorted by Separation");
				break;
			case 4:
				lstBinaries.get_Items().Add((object)"Sorted by Major axis of satellite");
				break;
			case 5:
				lstBinaries.get_Items().Add((object)"Discovery events, sortd by CBET number");
				break;
			}
			lstBinaries.get_Items().Add((object)"");
			for (int i = 0; i < Asteroid_Observations_Reports.BinaryList.Count; i++)
			{
				if (!(listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem.get_Checked() & (Asteroid_Observations_Reports.BinaryList[i].CBET == "")))
				{
					string[] array = Asteroid_Observations_Reports.BinaryList[i].ToString().Split(new char[1] { '|' });
					num++;
					array[0] = string.Format("#{0,3}  ", num) + array[0];
					for (int j = 0; j < array.Length; j++)
					{
						lstBinaries.get_Items().Add((object)array[j]);
					}
					lstBinaries.get_Items().Add((object)"");
				}
			}
		}

		private void ListBinaryAsteroid_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() < 200)
			{
				((Control)this).set_Width(200);
			}
			if (((Control)this).get_Height() < 200)
			{
				((Control)this).set_Height(200);
			}
			((Control)lstBinaries).set_Width(((Control)this).get_Width() - 25);
			((Control)lstBinaries).set_Height(((Control)this).get_Height() - 81);
		}

		private void listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem.set_Checked(!listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem.get_Checked());
			Display();
		}

		private void lstBinaries_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() != 2097152)
			{
				return;
			}
			try
			{
				string text = lstBinaries.get_SelectedItem().ToString();
				if ((text.Substring(0, 1) != "#") | (((ListControl)lstBinaries).get_SelectedIndex() > lstBinaries.get_Items().get_Count() - 3))
				{
					ErrorMessage(((Control)this).get_Left(), ((Control)this).get_Top());
					return;
				}
				string asteroidNumber = text.Substring(7, 7);
				string asteroidName = text.PadRight(32).Substring(15, 16).Trim();
				string text2 = lstBinaries.get_Items().get_Item(((ListControl)lstBinaries).get_SelectedIndex() + 1).ToString();
				if (text2.Substring(0, 3) == "***")
				{
					text2 = lstBinaries.get_Items().get_Item(((ListControl)lstBinaries).get_SelectedIndex() + 2).ToString();
				}
				int.TryParse(text2.Substring(2, 4), out var result);
				int month = 1;
				string text3 = text2.Substring(7, 3);
				for (int i = 1; i < 13; i++)
				{
					if (Utilities.ShortMonths[i] == text3)
					{
						month = i;
					}
				}
				int.TryParse(text2.Substring(11, 2), out var result2);
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
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_016f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0179: Expected O, but got Unknown
			lstBinaries = new ListBox();
			menuStrip1 = new MenuStrip();
			withBinaryAsteroidsToolStripMenuItem = new ToolStripMenuItem();
			listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copySelectedToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			sortToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNameToolStripMenuItem = new ToolStripMenuItem();
			byAsteroidNumberToolStripMenuItem = new ToolStripMenuItem();
			byDateToolStripMenuItem = new ToolStripMenuItem();
			bySeparationToolStripMenuItem = new ToolStripMenuItem();
			byMajorAxisOfCompanionToolStripMenuItem = new ToolStripMenuItem();
			byCBETNumberToolStripMenuItem = new ToolStripMenuItem();
			label5 = new Label();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstBinaries).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstBinaries).set_FormattingEnabled(true);
			lstBinaries.set_ItemHeight(14);
			((Control)lstBinaries).set_Location(new Point(1, 35));
			((Control)lstBinaries).set_Name("lstBinaries");
			lstBinaries.set_SelectionMode((SelectionMode)3);
			((Control)lstBinaries).set_Size(new Size(573, 354));
			((Control)lstBinaries).set_TabIndex(2);
			((Control)lstBinaries).add_MouseDown(new MouseEventHandler(lstBinaries_MouseDown));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withBinaryAsteroidsToolStripMenuItem,
				(ToolStripItem)sortToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(582, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withBinaryAsteroidsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[7]
			{
				(ToolStripItem)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copySelectedToolStripMenuItem,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withBinaryAsteroidsToolStripMenuItem).set_Name("withBinaryAsteroidsToolStripMenuItem");
			((ToolStripItem)withBinaryAsteroidsToolStripMenuItem).set_Size(new Size(137, 20));
			((ToolStripItem)withBinaryAsteroidsToolStripMenuItem).set_Text("with Binary asteroids...");
			((ToolStripItem)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem).set_BackColor(Color.Honeydew);
			((ToolStripItem)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem).set_Name("listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem");
			((ToolStripItem)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem).set_Text("List Discoveries with their CBET number");
			((ToolStripItem)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem).add_Click((EventHandler)listDiscoveriesWithCBETIdentifinumbersToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(279, 6));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Name("copySelectedToolStripMenuItem");
			((ToolStripItem)copySelectedToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)copySelectedToolStripMenuItem).set_Text("Copy selected");
			((ToolStripItem)copySelectedToolStripMenuItem).add_Click((EventHandler)copySelectedToolStripMenuItem_Click);
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(282, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripDropDownItem)sortToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)byAsteroidNameToolStripMenuItem,
				(ToolStripItem)byAsteroidNumberToolStripMenuItem,
				(ToolStripItem)byDateToolStripMenuItem,
				(ToolStripItem)bySeparationToolStripMenuItem,
				(ToolStripItem)byMajorAxisOfCompanionToolStripMenuItem,
				(ToolStripItem)byCBETNumberToolStripMenuItem
			});
			((ToolStripItem)sortToolStripMenuItem).set_Image((Image)Resources.Sort);
			((ToolStripItem)sortToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)sortToolStripMenuItem).set_Name("sortToolStripMenuItem");
			((ToolStripItem)sortToolStripMenuItem).set_Size(new Size(65, 20));
			((ToolStripItem)sortToolStripMenuItem).set_Text("Sort...");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Name("byAsteroidNameToolStripMenuItem");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)byAsteroidNameToolStripMenuItem).set_Text("by Asteroid name");
			((ToolStripItem)byAsteroidNameToolStripMenuItem).add_Click((EventHandler)byAsteroidNameToolStripMenuItem_Click);
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Name("byAsteroidNumberToolStripMenuItem");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).set_Text("by Asteroid number");
			((ToolStripItem)byAsteroidNumberToolStripMenuItem).add_Click((EventHandler)byAsteroidNumberToolStripMenuItem_Click);
			((ToolStripItem)byDateToolStripMenuItem).set_Name("byDateToolStripMenuItem");
			((ToolStripItem)byDateToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)byDateToolStripMenuItem).set_Text("by Date");
			((ToolStripItem)byDateToolStripMenuItem).add_Click((EventHandler)byDateToolStripMenuItem_Click);
			((ToolStripItem)bySeparationToolStripMenuItem).set_Name("bySeparationToolStripMenuItem");
			((ToolStripItem)bySeparationToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)bySeparationToolStripMenuItem).set_Text("by Separation");
			((ToolStripItem)bySeparationToolStripMenuItem).add_Click((EventHandler)bySeparationToolStripMenuItem_Click);
			((ToolStripItem)byMajorAxisOfCompanionToolStripMenuItem).set_Name("byMajorAxisOfCompanionToolStripMenuItem");
			((ToolStripItem)byMajorAxisOfCompanionToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)byMajorAxisOfCompanionToolStripMenuItem).set_Text("by Major axis of companion");
			((ToolStripItem)byMajorAxisOfCompanionToolStripMenuItem).add_Click((EventHandler)byMajorAxisOfCompanionToolStripMenuItem_Click);
			((ToolStripItem)byCBETNumberToolStripMenuItem).set_Name("byCBETNumberToolStripMenuItem");
			((ToolStripItem)byCBETNumberToolStripMenuItem).set_Size(new Size(222, 22));
			((ToolStripItem)byCBETNumberToolStripMenuItem).set_Text("by CBET number");
			((ToolStripItem)byCBETNumberToolStripMenuItem).add_Click((EventHandler)byCBETNumberToolStripMenuItem_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_BackColor(Color.MistyRose);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_ForeColor(Color.DarkBlue);
			((Control)label5).set_Location(new Point(300, 9));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(258, 13));
			((Control)label5).set_TabIndex(16);
			((Control)label5).set_Text("Right-click on event line to display the event");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(582, 396));
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)lstBinaries);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("ListBinaryAsteroid");
			((Control)this).set_Text("Binary Asteroids");
			((Control)this).add_Resize((EventHandler)ListBinaryAsteroid_Resize);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
