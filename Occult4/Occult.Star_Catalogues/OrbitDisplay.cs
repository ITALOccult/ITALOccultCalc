using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class OrbitDisplay : Form
	{
		private IContainer components;

		private ListBox lstOrbits;

		private Label label1;

		private PictureBox picOrbit;

		private Label label2;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem withOrbitDisplayToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		public OrbitDisplay()
		{
			InitializeComponent();
		}

		private void OrbitDisplay_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text("Orbit display for binary stars : Occult v." + Utilities.OccultVersion_Short);
		}

		private void lstOrbits_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			DisplayOrbit(lstOrbits.get_Items().get_Item(((ListControl)lstOrbits).get_SelectedIndex()).ToString());
		}

		internal void InitialDisplay()
		{
			lstOrbits.get_Items().Clear();
			for (int i = 0; i < Interferometric_Plus_WDS.Orbits.Count; i++)
			{
				lstOrbits.get_Items().Add((object)Interferometric_Plus_WDS.Orbits[i]!.ToString());
			}
			lstOrbits.set_Sorted(true);
			for (int j = 0; j < lstOrbits.get_Items().get_Count() - 1; j++)
			{
				while (j < lstOrbits.get_Items().get_Count() - 1 && lstOrbits.get_Items().get_Item(j).ToString() == lstOrbits.get_Items().get_Item(j + 1).ToString())
				{
					lstOrbits.get_Items().RemoveAt(j + 1);
				}
			}
			((ListControl)lstOrbits).set_SelectedIndex(0);
			DisplayOrbit(lstOrbits.get_Items().get_Item(0).ToString());
		}

		internal void DisplayOrbit(string Address)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a8: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			string text = Settings.Default.Orb6DownloadServer + "/PNG/" + Address;
			try
			{
				picOrbit.Load(text);
				((Control)picOrbit).set_Width(picOrbit.get_Image().Width);
				((Control)picOrbit).set_Height(picOrbit.get_Image().Height);
				((Control)this).set_Height(((Control)picOrbit).get_Height() + 180);
				((Control)this).set_Width(((Control)picOrbit).get_Width() + 38);
			}
			catch
			{
				MessageBox.Show("Requested orbit is not available.", "No orbit");
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"6th Orbit Catalog Display");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picOrbit.get_Image());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstOrbits).get_SelectedIndex() >= 0)
			{
				Output.SaveGraphic(picOrbit.get_Image(), lstOrbits.get_Items().get_Item(((ListControl)lstOrbits).get_SelectedIndex()).ToString(), Utilities.AppPath + "\\Predictions");
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
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			lstOrbits = new ListBox();
			label1 = new Label();
			picOrbit = new PictureBox();
			label2 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			withOrbitDisplayToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			((ISupportInitialize)picOrbit).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ListControl)lstOrbits).set_FormattingEnabled(true);
			((Control)lstOrbits).set_Location(new Point(242, 50));
			((Control)lstOrbits).set_Name("lstOrbits");
			((Control)lstOrbits).set_Size(new Size(159, 56));
			((Control)lstOrbits).set_TabIndex(0);
			((Control)lstOrbits).add_MouseDoubleClick(new MouseEventHandler(lstOrbits_MouseDoubleClick));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(266, 32));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(110, 13));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Available orbits to plot");
			((Control)picOrbit).set_Location(new Point(11, 132));
			((Control)picOrbit).set_Name("picOrbit");
			((Control)picOrbit).set_Size(new Size(608, 425));
			picOrbit.set_TabIndex(2);
			picOrbit.set_TabStop(false);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(280, 109));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(83, 12));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Double-click to plot");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withOrbitDisplayToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(642, 24));
			((Control)menuStrip1).set_TabIndex(4);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ToolStripDropDownItem)withOrbitDisplayToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withOrbitDisplayToolStripMenuItem).set_Name("withOrbitDisplayToolStripMenuItem");
			((ToolStripItem)withOrbitDisplayToolStripMenuItem).set_Size(new Size(130, 20));
			((ToolStripItem)withOrbitDisplayToolStripMenuItem).set_Text("with Orbit display...   ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(152, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(642, 569));
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)picOrbit);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstOrbits);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("OrbitDisplay");
			((Control)this).set_Text("OrbitDisplay");
			((Form)this).add_Load((EventHandler)OrbitDisplay_Load);
			((ISupportInitialize)picOrbit).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
