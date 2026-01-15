using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class AsteroidSelectForSearch : Form
	{
		public int FirstRecord;

		public int LastRecord;

		public int FirstAsteroidNumber;

		public int LastAsteroidNumber;

		public string FirstAsteroidName = "";

		public string LastAsteroidName = "";

		public string SearchLabel = "None meeting criteria";

		public int NumberOfAsteroids = -1;

		private bool UserElements;

		private string SelectionString = "*";

		private string SelectionEntered = "";

		private bool SelectionStartsWith;

		private bool SelectionEndsWith;

		private bool SelectionContains;

		private bool SelectionIs = true;

		private List<int> RecNumIndex = new List<int>();

		public string FirstMP = "";

		public string LastMP = "";

		public string SearchString;

		public string AsteroidNumberSearchString;

		public string AsteroidNumberSearchString2;

		public int FirstMPRecord;

		public int LastMPRecord;

		public int SearchAsteroidNumber;

		public int SearchAsteroidNumber2;

		private ArrayList FileIndex = new ArrayList();

		private IContainer components;

		private ListBox lstAsteroids;

		private Button cmdOK;

		private Label lblOK1;

		private Button cmdHelp;

		private Button cmdCancel;

		private Label lblOK2;

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
		}

		public AsteroidSelectForSearch(bool UseUserElements, string Selection)
		{
			UserElements = UseUserElements;
			SelectionStartsWith = (SelectionEndsWith = (SelectionContains = (SelectionIs = false)));
			SelectionEntered = Selection;
			SelectionString = Selection.Replace("*", "").ToUpper();
			if (!Selection.Contains("*"))
			{
				SelectionIs = true;
			}
			else if (Selection.EndsWith("*") & Selection.StartsWith("*"))
			{
				SelectionContains = true;
			}
			else if (Selection.EndsWith("*"))
			{
				SelectionStartsWith = true;
			}
			else if (Selection.StartsWith("*"))
			{
				SelectionEndsWith = true;
			}
			InitializeComponent();
			string text;
			if (Selection.Contains("*"))
			{
				lstAsteroids.set_SelectionMode((SelectionMode)1);
				text = "Select a single asteroid";
			}
			else
			{
				lstAsteroids.set_SelectionMode((SelectionMode)3);
				text = "Select a continuous range of asteroids";
			}
			if (UserElements)
			{
				((Control)this).set_Text("Minor planet selector:   USER file of asteroid elements - " + text);
			}
			else
			{
				((Control)this).set_Text("Minor planet selector:   MAIN file of asteroid elements - " + text);
			}
		}

		private void AsteroidSelectForSearch_Load(object sender, EventArgs e)
		{
			string text = "";
			string text2 = "";
			((Control)lblOK2).set_Top(((Control)lblOK1).get_Top());
			((Control)lblOK1).set_Visible(SelectionIs);
			((Control)lblOK2).set_Visible(!((Control)lblOK1).get_Visible());
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (UserElements)
			{
				if (Elements.UserAsteroids.AstElements.Count < 5)
				{
					Elements.UserAsteroids.Fill_AllAsteroids();
				}
				else if (Elements.MainAsteroids.AstElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			if (UserElements)
			{
				for (int i = 0; i < Elements.UserAsteroids.AstElements.Count; i++)
				{
					text2 = Elements.UserAsteroids.AstElements[i].IDName.ToUpper();
					if (SelectionContains)
					{
						if (!text2.Contains(SelectionString))
						{
							continue;
						}
					}
					else if (SelectionStartsWith)
					{
						if (!text2.StartsWith(SelectionString))
						{
							continue;
						}
					}
					else if (SelectionEndsWith && !text2.EndsWith(SelectionString))
					{
						continue;
					}
					text = Elements.UserAsteroids.AstElements[i].IDNumber.ToString();
					if (text == "0")
					{
						text = " ";
					}
					lstAsteroids.get_Items().Add((object)(text.PadLeft(7) + " " + Elements.UserAsteroids.AstElements[i].IDName.PadRight(16)));
					RecNumIndex.Add(i);
				}
			}
			else
			{
				for (int j = 0; j < Elements.MainAsteroids.AstElements.Count; j++)
				{
					text2 = Elements.MainAsteroids.AstElements[j].IDName.ToUpper();
					if (SelectionContains)
					{
						if (!text2.Contains(SelectionString))
						{
							continue;
						}
					}
					else if (SelectionStartsWith)
					{
						if (!text2.StartsWith(SelectionString))
						{
							continue;
						}
					}
					else if (SelectionEndsWith && !text2.EndsWith(SelectionString))
					{
						continue;
					}
					text = Elements.MainAsteroids.AstElements[j].IDNumber.ToString();
					if (text == "0")
					{
						text = " ";
					}
					lstAsteroids.get_Items().Add((object)(text.PadLeft(7) + " " + Elements.MainAsteroids.AstElements[j].IDName.PadRight(16)));
					RecNumIndex.Add(j);
				}
			}
			((Control)this).set_Height((int)(0.6 * (double)Screen.GetWorkingArea((Control)(object)this).Height));
		}

		private void AsteroidSelectForSearch_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 100) | (((Control)this).get_Width() < 150)))
			{
				((Control)lstAsteroids).set_Width(((Control)this).get_Width() - 15);
				((Control)lstAsteroids).set_Height(((Control)this).get_Height() - 73);
			}
		}

		private void lstAsteroids_SelectedIndexChanged(object sender, EventArgs e)
		{
			FirstMPRecord = lstAsteroids.get_SelectedIndices().get_Item(0);
			FirstMP = lstAsteroids.get_Items().get_Item(FirstMPRecord).ToString();
			LastMPRecord = lstAsteroids.get_SelectedIndices().get_Item(lstAsteroids.get_SelectedIndices().get_Count() - 1);
			LastMP = lstAsteroids.get_Items().get_Item(LastMPRecord).ToString();
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			if (lstAsteroids.get_Items().get_Count() == 0)
			{
				MessageBox.Show("No asteroids listed for:  " + SelectionEntered, "Data entry error", (MessageBoxButtons)0, (MessageBoxIcon)64);
				((Form)this).set_DialogResult((DialogResult)2);
				((Form)this).Close();
				return;
			}
			if (lstAsteroids.get_SelectedIndices().get_Count() < 1)
			{
				MessageBox.Show("No asteroids selected", "Data entry error", (MessageBoxButtons)0, (MessageBoxIcon)64);
				((Form)this).set_DialogResult((DialogResult)2);
				((Form)this).Close();
				return;
			}
			if (!SelectionIs)
			{
				FirstRecord = (LastRecord = RecNumIndex[lstAsteroids.get_SelectedIndices().get_Item(0)]);
				string text = lstAsteroids.get_Items().get_Item(lstAsteroids.get_SelectedIndices().get_Item(0)).ToString();
				string text2 = text.Substring(0, 8).Trim();
				if (text2 == "")
				{
					FirstAsteroidNumber = (LastAsteroidNumber = 0);
				}
				else
				{
					FirstAsteroidNumber = (LastAsteroidNumber = int.Parse(text2));
				}
				FirstAsteroidName = (LastAsteroidName = text.Substring(8).Trim());
				((Form)this).set_DialogResult((DialogResult)1);
				((Form)this).Close();
				return;
			}
			FirstRecord = lstAsteroids.get_SelectedIndices().get_Item(0);
			if (FirstRecord >= 0)
			{
				string text3 = lstAsteroids.get_Items().get_Item(FirstRecord).ToString();
				string text4 = text3.Substring(0, 8).Trim();
				if (text4 == "")
				{
					FirstAsteroidNumber = 0;
				}
				else
				{
					FirstAsteroidNumber = int.Parse(text4);
				}
				FirstAsteroidName = text3.Substring(8).Trim();
				LastRecord = lstAsteroids.get_SelectedIndices().get_Item(lstAsteroids.get_SelectedIndices().get_Count() - 1);
				text3 = lstAsteroids.get_Items().get_Item(LastRecord).ToString();
				text4 = text3.Substring(0, 8).Trim();
				if (text4 == "")
				{
					LastAsteroidNumber = 0;
				}
				else
				{
					LastAsteroidNumber = int.Parse(text4);
				}
				LastAsteroidName = text3.Substring(8).Trim();
				((Form)this).set_DialogResult((DialogResult)1);
				((Form)this).Close();
			}
		}

		private void lstAsteroids_DoubleClick(object sender, EventArgs e)
		{
			FirstRecord = (LastRecord = RecNumIndex[lstAsteroids.get_SelectedIndices().get_Item(0)]);
			string text = lstAsteroids.get_Items().get_Item(lstAsteroids.get_SelectedIndices().get_Item(0)).ToString();
			string text2 = text.Substring(0, 8).Trim();
			if (text2 == "")
			{
				FirstAsteroidNumber = (LastAsteroidNumber = 0);
			}
			else
			{
				FirstAsteroidNumber = (LastAsteroidNumber = int.Parse(text2));
			}
			FirstAsteroidName = (LastAsteroidName = text.Substring(8).Trim());
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - select");
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_046d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0477: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidSelectForSearch));
			lstAsteroids = new ListBox();
			cmdOK = new Button();
			lblOK1 = new Label();
			cmdHelp = new Button();
			cmdCancel = new Button();
			lblOK2 = new Label();
			((Control)this).SuspendLayout();
			lstAsteroids.set_ColumnWidth(120);
			((ListControl)lstAsteroids).set_FormattingEnabled(true);
			((Control)lstAsteroids).set_Location(new Point(-1, -1));
			lstAsteroids.set_MultiColumn(true);
			((Control)lstAsteroids).set_Name("lstAsteroids");
			lstAsteroids.set_SelectionMode((SelectionMode)3);
			((Control)lstAsteroids).set_Size(new Size(946, 550));
			((Control)lstAsteroids).set_TabIndex(0);
			lstAsteroids.add_SelectedIndexChanged((EventHandler)lstAsteroids_SelectedIndexChanged);
			((Control)lstAsteroids).add_DoubleClick((EventHandler)lstAsteroids_DoubleClick);
			((Control)cmdOK).set_Anchor((AnchorStyles)2);
			((Control)cmdOK).set_Location(new Point(732, 557));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(55, 26));
			((Control)cmdOK).set_TabIndex(3);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)lblOK1).set_Anchor((AnchorStyles)2);
			((Control)lblOK1).set_AutoSize(true);
			((Control)lblOK1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblOK1).set_Location(new Point(9, 565));
			((Control)lblOK1).set_Name("lblOK1");
			((Control)lblOK1).set_Size(new Size(717, 13));
			((Control)lblOK1).set_TabIndex(9);
			((Control)lblOK1).set_Text("Double-click on a single minor planet   - OR -   highlight the continuous range of minor planets to be included, and click  'OK'");
			((Control)cmdHelp).set_Anchor((AnchorStyles)2);
			((Control)cmdHelp).set_Location(new Point(884, 557));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(55, 26));
			((Control)cmdHelp).set_TabIndex(10);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)2);
			((Control)cmdCancel).set_Location(new Point(808, 558));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(55, 26));
			((Control)cmdCancel).set_TabIndex(11);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)lblOK2).set_Anchor((AnchorStyles)2);
			((Control)lblOK2).set_AutoSize(true);
			((Control)lblOK2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblOK2).set_Location(new Point(194, 552));
			((Control)lblOK2).set_Name("lblOK2");
			((Control)lblOK2).set_Size(new Size(346, 13));
			((Control)lblOK2).set_TabIndex(12);
			((Control)lblOK2).set_Text("Select an asteroid, then either Double-click  OR  click  'OK'");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(944, 589));
			((Control)this).get_Controls().Add((Control)(object)lblOK2);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)lblOK1);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)lstAsteroids);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterSelectAsteroids", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterSelectAsteroids);
			((Control)this).set_Name("AsteroidSelectForSearch");
			((Control)this).set_Text("Minor planet selector - select a single minor planet, or a continuous range of minor planets");
			((Form)this).add_Load((EventHandler)AsteroidSelectForSearch_Load);
			((Control)this).add_Resize((EventHandler)AsteroidSelectForSearch_Resize);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
