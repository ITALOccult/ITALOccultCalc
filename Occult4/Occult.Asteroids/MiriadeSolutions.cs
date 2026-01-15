using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class MiriadeSolutions : Form
	{
		private IContainer components;

		private ListBox lstSolutions;

		private ListBox lstMiriadeAsteroids;

		private Panel panel1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label label1;

		public MiriadeSolutions()
		{
			InitializeComponent();
		}

		private void MiriadeSolutions_Load(object sender, EventArgs e)
		{
			string[] array = Settings.Default.MiriadeAsteroids.Split(new char[1] { ' ' });
			lstMiriadeAsteroids.get_Items().Clear();
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Length > 0)
				{
					lstMiriadeAsteroids.get_Items().Add((object)array[i]);
				}
			}
			if (lstMiriadeAsteroids.get_Items().get_Count() >= 0)
			{
				((ListControl)lstMiriadeAsteroids).set_SelectedIndex(0);
			}
			((Control)panel1).set_Height(((Control)this).get_Height() - 66);
			((Control)lstSolutions).set_Height(((Control)panel1).get_Height() - 20);
			((Control)lstMiriadeAsteroids).set_Height(((Control)lstSolutions).get_Height() - 10);
		}

		private void lstMiriadeAsteroids_SelectedIndexChanged(object sender, EventArgs e)
		{
			new List<int>();
			((Control)lstSolutions).set_Width(1118);
			if (BinaryAsteroids.BinElements.Count < 1)
			{
				BinaryAsteroids.Fill_AllAsteroids();
			}
			int num = int.Parse(lstMiriadeAsteroids.get_Items().get_Item(((ListControl)lstMiriadeAsteroids).get_SelectedIndex()).ToString());
			Cursor.set_Current(Cursors.get_WaitCursor());
			string[] array = http.Download_WebPage("https://ssp.imcce.fr/webservices/miriade/api/ephemsys.php?-get=gensol&-name=" + num).Split(new char[1] { '\n' });
			lstSolutions.get_Items().Clear();
			lstSolutions.get_Items().Add((object)"Sn     System               Component      Solution    Fit   Prob     Radius      Solution          Eph    Ephemeris Range       Err     Error available");
			lstSolutions.get_Items().Add((object)" #  Number Name             Name           Method      mas     %     km   ±km     Date & Time        ?     Start       End        ?      Start       End");
			lstSolutions.get_Items().Add((object)"");
			for (int i = 1; i < array.Length; i++)
			{
				if (array[i].Trim().Length > 0)
				{
					string[] array2 = array[i].Split(new char[1] { ',' });
					StringBuilder stringBuilder = new StringBuilder();
					stringBuilder.Append(array2[0].PadLeft(2));
					stringBuilder.Append(array2[1].PadLeft(8));
					stringBuilder.Append(array2[2].PadRight(17));
					stringBuilder.Append(array2[3].PadRight(15));
					stringBuilder.Append(array2[4].PadRight(11));
					stringBuilder.Append(array2[5].PadLeft(6));
					stringBuilder.Append(array2[6].PadLeft(5));
					stringBuilder.Append(array2[7].PadLeft(8));
					stringBuilder.Append(array2[8].PadLeft(6));
					stringBuilder.Append("   " + array2[9].PadRight(16).Substring(0, 16));
					if (array2[10].Trim() == "1")
					{
						stringBuilder.Append("    y   ");
					}
					else
					{
						stringBuilder.Append("    n   ");
					}
					stringBuilder.Append(array2[11].PadRight(10).Substring(0, 10));
					stringBuilder.Append(" " + array2[12].PadRight(10).Substring(0, 10));
					if (array2[13].Trim() == "1")
					{
						stringBuilder.Append("    y   ");
						stringBuilder.Append(" " + array2[14].PadRight(10).Substring(0, 10));
						stringBuilder.Append(" " + array2[15].PadRight(10).Substring(0, 10));
					}
					else
					{
						stringBuilder.Append("    n   ");
					}
					lstSolutions.get_Items().Add((object)stringBuilder.ToString());
				}
			}
			Cursor.set_Current(((Control)this).get_DefaultCursor());
		}

		private void MiriadeSolutions_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() > 1220)
			{
				((Control)this).set_Width(1220);
			}
			if (((Control)this).get_Width() > 200)
			{
				((Control)panel1).set_Width(((Control)this).get_Width() - ((Control)panel1).get_Left() - 22);
			}
			if (((Control)this).get_Height() > 100)
			{
				((Control)panel1).set_Height(((Control)this).get_Height() - 66);
				((Control)lstSolutions).set_Height(((Control)panel1).get_Height() - 20);
				((Control)lstMiriadeAsteroids).set_Height(((Control)lstSolutions).get_Height() - 10);
			}
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string text = "";
			for (int i = 0; i < lstSolutions.get_Items().get_Count(); i++)
			{
				text = text + lstSolutions.get_Items().get_Item(i).ToString() + "\r\n";
			}
			Clipboard.SetText(text);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Miriade solutions");
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
			lstSolutions = new ListBox();
			lstMiriadeAsteroids = new ListBox();
			panel1 = new Panel();
			menuStrip1 = new MenuStrip();
			copyToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			label1 = new Label();
			((Control)panel1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstSolutions).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSolutions).set_FormattingEnabled(true);
			lstSolutions.set_HorizontalExtent(400);
			lstSolutions.set_HorizontalScrollbar(true);
			lstSolutions.set_ItemHeight(14);
			((Control)lstSolutions).set_Location(new Point(3, 6));
			((Control)lstSolutions).set_Name("lstSolutions");
			((Control)lstSolutions).set_Size(new Size(700, 186));
			((Control)lstSolutions).set_TabIndex(0);
			((ListControl)lstMiriadeAsteroids).set_FormattingEnabled(true);
			((Control)lstMiriadeAsteroids).set_Location(new Point(6, 41));
			((Control)lstMiriadeAsteroids).set_Name("lstMiriadeAsteroids");
			((Control)lstMiriadeAsteroids).set_RightToLeft((RightToLeft)1);
			lstMiriadeAsteroids.set_ScrollAlwaysVisible(true);
			((Control)lstMiriadeAsteroids).set_Size(new Size(71, 121));
			((Control)lstMiriadeAsteroids).set_TabIndex(1);
			lstMiriadeAsteroids.add_SelectedIndexChanged((EventHandler)lstMiriadeAsteroids_SelectedIndexChanged);
			((ScrollableControl)panel1).set_AutoScroll(true);
			((Control)panel1).get_Controls().Add((Control)(object)lstSolutions);
			((Control)panel1).set_Location(new Point(80, 23));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(709, 216));
			((Control)panel1).set_TabIndex(2);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(794, 24));
			((Control)menuStrip1).set_TabIndex(3);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy   ");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(33, 27));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(45, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Asteroid");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(794, 243));
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)lstMiriadeAsteroids);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("MiriadeSolutions");
			((Control)this).set_Text("Miriade solutions for a Binary Asteroid");
			((Form)this).add_Load((EventHandler)MiriadeSolutions_Load);
			((Control)this).add_Resize((EventHandler)MiriadeSolutions_Resize);
			((Control)panel1).ResumeLayout(false);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
