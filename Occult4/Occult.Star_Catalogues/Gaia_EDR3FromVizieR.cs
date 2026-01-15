using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class Gaia_EDR3FromVizieR : Form
	{
		internal List<StarEntry> stars = new List<StarEntry>();

		internal int SelectedStar = -10;

		private VizierMirror Vizier;

		private RadioButton[] radioButtons = (RadioButton[])(object)new RadioButton[5];

		private IContainer components;

		private Label label1;

		private Label label29;

		internal TextBox txtm;

		internal TextBox txtD;

		private Label label5;

		internal TextBox txtRAs;

		internal TextBox txts;

		internal TextBox txtRAm;

		internal TextBox txtRAh;

		private Label label16;

		private Label label13;

		private Label label12;

		private Label label2;

		private Button cmdGetGaiaEDR3;

		private Label label3;

		private Label label4;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		public TextBox txtU4Zone;

		public TextBox txtU4SeqNum;

		private Label label10;

		private Button cmdReturn;

		internal Button cmdGetU4;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem vizierSiteToolStripMenuItem;

		private ToolStripMenuItem franceToolStripMenuItem;

		private ToolStripMenuItem uSAToolStripMenuItem;

		private ToolStripMenuItem canadaToolStripMenuItem;

		private ToolStripMenuItem japanToolStripMenuItem;

		private ToolStripMenuItem indiaToolStripMenuItem;

		private ToolStripMenuItem chinaToolStripMenuItem;

		private ToolStripMenuItem uKToolStripMenuItem;

		private ToolStripMenuItem hawaiiToolStripMenuItem;

		private ToolStripMenuItem russiaToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		public Gaia_EDR3FromVizieR(string Zone, string SeqNo, string Label, string GRA, string Gdec)
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_003f: Expected O, but got Unknown
			InitializeComponent();
			for (int i = 0; i < 5; i++)
			{
				radioButtons[i] = new RadioButton();
				((Control)radioButtons[i]).set_Text("none");
				((Control)radioButtons[i]).set_AutoSize(true);
				radioButtons[i].set_Checked(i == 0);
				((Control)radioButtons[i]).set_Visible(i == 0);
				if (i == 0)
				{
					((Control)radioButtons[i]).set_Location(new Point(15, 150));
				}
				else
				{
					((Control)radioButtons[i]).set_Location(new Point(15, 110 + 80 * i));
				}
				((Control)this).get_Controls().Add((Control)(object)radioButtons[i]);
				radioButtons[i].add_CheckedChanged((EventHandler)RadioButton_CheckedChanged);
			}
			if (Zone != "")
			{
				((Control)txtU4Zone).set_Text(Zone);
				((Control)txtU4SeqNum).set_Text(SeqNo);
				((Control)cmdGetU4).set_Text(Label);
			}
			else
			{
				((Control)txtRAh).set_Text(GRA.Substring(0, 2));
				((Control)txtRAm).set_Text(GRA.Substring(2, 2));
				((Control)txtRAs).set_Text(GRA.Substring(4, 4));
				((Control)txtD).set_Text(Gdec.Substring(0, 3));
				((Control)txtm).set_Text(Gdec.Substring(3, 2));
				((Control)txts).set_Text(Gdec.Substring(5));
			}
			setVizierChecks(Settings.Default.VizierServer);
		}

		private void RadioButton_CheckedChanged(object sender, EventArgs e)
		{
		}

		private void cmdGetU4_Click(object sender, EventArgs e)
		{
			double num = 16.0;
			bool flag = false;
			if (int.TryParse(((Control)txtU4Zone).get_Text(), out var result) && int.TryParse(((Control)txtU4SeqNum).get_Text(), out var result2) && (((Control)cmdGetU4).get_Text().Contains("USNO-B1") ? GetStarPosition.GetB1Position(result, result2, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _) : ((!((Control)cmdGetU4).get_Text().Contains("Tycho-2)")) ? GetStarPosition.GetUCAC4Position(result, result2, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out var Parallax, out var UsedGaia2, out var Epoch2) : GetStarPosition.GetTycho2Position(result, result2, 1, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia2, out Epoch2))))
			{
				string text = Utilities.DEGtoDMS((RA + num * pmRA) * (180.0 / Math.PI) / 15.0, 2, 2, MinutesOnly: false);
				string text2 = Utilities.DEGtoDMS((Dec + num * pmDec) * (180.0 / Math.PI), 3, 1, MinutesOnly: false);
				((Control)txtRAh).set_Text(text.Substring(0, 2));
				((Control)txtRAm).set_Text(text.Substring(3, 2));
				((Control)txtRAs).set_Text(text.Substring(6));
				((Control)txtD).set_Text(text2.Substring(0, 3));
				((Control)txtm).set_Text(text2.Substring(4, 2));
				((Control)txts).set_Text(text2.Substring(7));
			}
		}

		private void cmdGetGaiaDR2_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			for (int i = 0; i < 5; i++)
			{
				((Control)radioButtons[i]).set_Text("none");
				radioButtons[i].set_Checked(i == 0);
				((Control)radioButtons[i]).set_Visible(i == 0);
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			if (!int.TryParse(((Control)txtRAh).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtRAm).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtRAs).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			double num = (double)result + (double)result2 / 60.0 + result3 / 3600.0;
			if (!int.TryParse(((Control)txtD).get_Text().Replace("-", ""), out result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtm).get_Text(), out result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txts).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			double num2 = (double)result + (double)result2 / 60.0 + result3 / 3600.0;
			if (((Control)txtD).get_Text().Contains("-"))
			{
				num2 = 0.0 - num2;
			}
			if (num == 0.0 || num2 == 0.0)
			{
				MessageBox.Show("You have not set the approximate star coordinates for the VizieR search. Either specify a UCAC4 number and click the associated button, or manually enter the coordinates.", "No coordinates", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			stars = CatalogQuery.GetStarsInRegion(num, num2, 4, VizierEquinox.J2000, "I/350", Vizier);
			int num3 = 1;
			foreach (StarEntry star in stars)
			{
				string text = "    Position :     RA = " + Utilities.DEGtoDMS(star.RACat, 2, 5, MinutesOnly: false) + "     Dec = " + Utilities.DEGtoDMS(star.DECat, 3, 4, MinutesOnly: false);
				string text2 = string.Format("    Magnitudes :     Mv = {0,5:f2}     Mb = {1,5:f2}     Mr = {2,5:f2}", star.Mv, star.Mb, star.Mr);
				string text3 = string.Format("    Proper motions :     RA = {0,6:f6}s     Dec = {1,5:f6}\"\r\n    Parallax, RV : Parx = {2,6:f4}mas   RV = {3,4:f1}km/sec\r\n   RUWE = {4,4:f3}", star.pmRA, star.pmDEC, star.Parallax / 1000.0, star.RV, star.RUWE);
				((Control)radioButtons[num3]).set_Text(text + "\r\n" + text2 + "\r\n" + text3);
				((Control)radioButtons[num3]).set_Visible(true);
				num3++;
				if (num3 > 4)
				{
					break;
				}
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
		}

		private void cmdReturn_Click(object sender, EventArgs e)
		{
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			//IL_004a: Invalid comparison between Unknown and I4
			for (int i = 0; i < 5; i++)
			{
				if (radioButtons[i].get_Checked())
				{
					SelectedStar = i - 1;
				}
			}
			if (SelectedStar >= 0 || (int)MessageBox.Show("You have not selected a star. Do you want to return without updating the star position in the observations report to a Gaia DR2 position?", "No star selected", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Form)this).Close();
			}
		}

		private void franceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(0);
		}

		private void uSAToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(1);
		}

		private void canadaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(2);
		}

		private void japanToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(3);
		}

		private void indiaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(4);
		}

		private void chinaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(5);
		}

		private void uKToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(6);
		}

		private void hawaiiToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(7);
		}

		private void russiaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(8);
		}

		internal void setVizierChecks(int V)
		{
			Settings.Default.VizierServer = V;
			franceToolStripMenuItem.set_Checked(V == 0);
			uSAToolStripMenuItem.set_Checked(V == 1);
			canadaToolStripMenuItem.set_Checked(V == 2);
			japanToolStripMenuItem.set_Checked(V == 3);
			indiaToolStripMenuItem.set_Checked(V == 4);
			chinaToolStripMenuItem.set_Checked(V == 5);
			uKToolStripMenuItem.set_Checked(V == 6);
			hawaiiToolStripMenuItem.set_Checked(V == 7);
			russiaToolStripMenuItem.set_Checked(V == 8);
			switch (V)
			{
			case 1:
				Vizier = VizierMirror.USA;
				break;
			case 2:
				Vizier = VizierMirror.Canada;
				break;
			case 3:
				Vizier = VizierMirror.Japan;
				break;
			case 4:
				Vizier = VizierMirror.India;
				break;
			case 5:
				Vizier = VizierMirror.China;
				break;
			case 6:
				Vizier = VizierMirror.UK;
				break;
			case 7:
				Vizier = VizierMirror.Hawaii;
				break;
			case 8:
				Vizier = VizierMirror.Russia;
				break;
			default:
				Vizier = VizierMirror.France;
				break;
			}
			((Control)cmdGetGaiaEDR3).set_Text("Get Gaia EDR3\nfrom VizieR\nin " + Vizier);
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Get Gaia DR2 from VizieR");
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
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Gaia_EDR3FromVizieR));
			txtU4Zone = new TextBox();
			txtU4SeqNum = new TextBox();
			label1 = new Label();
			cmdGetU4 = new Button();
			label29 = new Label();
			txtm = new TextBox();
			txtD = new TextBox();
			label5 = new Label();
			txtRAs = new TextBox();
			txts = new TextBox();
			txtRAm = new TextBox();
			txtRAh = new TextBox();
			label16 = new Label();
			label13 = new Label();
			label12 = new Label();
			label2 = new Label();
			cmdGetGaiaEDR3 = new Button();
			label3 = new Label();
			label4 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			cmdReturn = new Button();
			menuStrip1 = new MenuStrip();
			vizierSiteToolStripMenuItem = new ToolStripMenuItem();
			franceToolStripMenuItem = new ToolStripMenuItem();
			uSAToolStripMenuItem = new ToolStripMenuItem();
			canadaToolStripMenuItem = new ToolStripMenuItem();
			japanToolStripMenuItem = new ToolStripMenuItem();
			indiaToolStripMenuItem = new ToolStripMenuItem();
			chinaToolStripMenuItem = new ToolStripMenuItem();
			uKToolStripMenuItem = new ToolStripMenuItem();
			hawaiiToolStripMenuItem = new ToolStripMenuItem();
			russiaToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtU4Zone).set_Location(new Point(9, 50));
			((Control)txtU4Zone).set_Name("txtU4Zone");
			((Control)txtU4Zone).set_Size(new Size(47, 20));
			((Control)txtU4Zone).set_TabIndex(0);
			((Control)txtU4SeqNum).set_Location(new Point(68, 50));
			((Control)txtU4SeqNum).set_Name("txtU4SeqNum");
			((Control)txtU4SeqNum).set_Size(new Size(69, 20));
			((Control)txtU4SeqNum).set_TabIndex(1);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(56, 53));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(11, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("-");
			((Control)cmdGetU4).set_Location(new Point(11, 74));
			((Control)cmdGetU4).set_Name("cmdGetU4");
			((Control)cmdGetU4).set_Size(new Size(126, 30));
			((Control)cmdGetU4).set_TabIndex(3);
			((Control)cmdGetU4).set_Text("Get UCAC4 position");
			((ButtonBase)cmdGetU4).set_UseVisualStyleBackColor(true);
			((Control)cmdGetU4).add_Click((EventHandler)cmdGetU4_Click);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(207, 85));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(27, 13));
			((Control)label29).set_TabIndex(35);
			((Control)label29).set_Text("Dec");
			((Control)txtm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtm).set_Location(new Point(267, 82));
			((Control)txtm).set_Name("txtm");
			((Control)txtm).set_Size(new Size(25, 20));
			((Control)txtm).set_TabIndex(32);
			((Control)txtm).set_Text("0");
			((Control)txtD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD).set_ImeMode((ImeMode)0);
			((Control)txtD).set_Location(new Point(235, 82));
			((Control)txtD).set_Name("txtD");
			((Control)txtD).set_Size(new Size(26, 20));
			((Control)txtD).set_TabIndex(30);
			((Control)txtD).set_Text("0");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(207, 54));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(22, 13));
			((Control)label5).set_TabIndex(25);
			((Control)label5).set_Text("RA");
			((Control)txtRAs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAs).set_Location(new Point(298, 50));
			((Control)txtRAs).set_Name("txtRAs");
			((Control)txtRAs).set_Size(new Size(48, 20));
			((Control)txtRAs).set_TabIndex(28);
			((Control)txtRAs).set_Text("0.0");
			((Control)txts).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txts).set_Location(new Point(298, 82));
			((Control)txts).set_Name("txts");
			((Control)txts).set_Size(new Size(41, 20));
			((Control)txts).set_TabIndex(34);
			((Control)txts).set_Text("0.0");
			((Control)txtRAm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAm).set_Location(new Point(267, 50));
			((Control)txtRAm).set_Name("txtRAm");
			((Control)txtRAm).set_Size(new Size(25, 20));
			((Control)txtRAm).set_TabIndex(27);
			((Control)txtRAm).set_Text("0");
			((Control)txtRAh).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAh).set_Location(new Point(235, 50));
			((Control)txtRAh).set_Name("txtRAh");
			((Control)txtRAh).set_Size(new Size(26, 20));
			((Control)txtRAh).set_TabIndex(26);
			((Control)txtRAh).set_Text("0");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(307, 75));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(12, 13));
			((Control)label16).set_TabIndex(33);
			((Control)label16).set_Text("\"");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(272, 75));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(9, 13));
			((Control)label13).set_TabIndex(31);
			((Control)label13).set_Text("'");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(243, 70));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(13, 13));
			((Control)label12).set_TabIndex(29);
			((Control)label12).set_Text("o");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(146, 54));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(65, 39));
			((Control)label2).set_TabIndex(36);
			((Control)label2).set_Text("OR enter \r\ncoordinates \r\nmanually");
			((Control)cmdGetGaiaEDR3).set_Location(new Point(372, 50));
			((Control)cmdGetGaiaEDR3).set_Name("cmdGetGaiaEDR3");
			((Control)cmdGetGaiaEDR3).set_Size(new Size(108, 54));
			((Control)cmdGetGaiaEDR3).set_TabIndex(37);
			((Control)cmdGetGaiaEDR3).set_Text("Get Gaia EDR3/DR3 from VizieR");
			((ButtonBase)cmdGetGaiaEDR3).set_UseVisualStyleBackColor(true);
			((Control)cmdGetGaiaEDR3).add_Click((EventHandler)cmdGetGaiaDR2_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(241, 36));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(13, 13));
			((Control)label3).set_TabIndex(38);
			((Control)label3).set_Text("h");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(272, 36));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(15, 13));
			((Control)label4).set_TabIndex(39);
			((Control)label4).set_Text("m");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(310, 36));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(12, 13));
			((Control)label6).set_TabIndex(40);
			((Control)label6).set_Text("s");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(12, 36));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(32, 13));
			((Control)label7).set_TabIndex(41);
			((Control)label7).set_Text("Zone");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(72, 36));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(43, 13));
			((Control)label8).set_TabIndex(42);
			((Control)label8).set_Text("Seq No");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(197, 23));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(174, 13));
			((Control)label9).set_TabIndex(43);
			((Control)label9).set_Text("Approximate coordinates for 2016.0");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(46, 119));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(288, 15));
			((Control)label10).set_TabIndex(44);
			((Control)label10).set_Text("Select the occulted star    [ Epoch is 2016.0]");
			((Control)cmdReturn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReturn).set_Location(new Point(178, 454));
			((Control)cmdReturn).set_Name("cmdReturn");
			((Control)cmdReturn).set_Size(new Size(137, 35));
			((Control)cmdReturn).set_TabIndex(45);
			((Control)cmdReturn).set_Text("Return selected star");
			((ButtonBase)cmdReturn).set_UseVisualStyleBackColor(true);
			((Control)cmdReturn).add_Click((EventHandler)cmdReturn_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)vizierSiteToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(492, 24));
			((Control)menuStrip1).set_TabIndex(46);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)vizierSiteToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)franceToolStripMenuItem,
				(ToolStripItem)uSAToolStripMenuItem,
				(ToolStripItem)canadaToolStripMenuItem,
				(ToolStripItem)japanToolStripMenuItem,
				(ToolStripItem)indiaToolStripMenuItem,
				(ToolStripItem)chinaToolStripMenuItem,
				(ToolStripItem)uKToolStripMenuItem,
				(ToolStripItem)hawaiiToolStripMenuItem,
				(ToolStripItem)russiaToolStripMenuItem
			});
			((ToolStripItem)vizierSiteToolStripMenuItem).set_Name("vizierSiteToolStripMenuItem");
			((ToolStripItem)vizierSiteToolStripMenuItem).set_Size(new Size(77, 20));
			((ToolStripItem)vizierSiteToolStripMenuItem).set_Text("Vizier site...");
			franceToolStripMenuItem.set_Checked(true);
			franceToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)franceToolStripMenuItem).set_Image((Image)Resources.FLGFRAN);
			((ToolStripItem)franceToolStripMenuItem).set_Name("franceToolStripMenuItem");
			((ToolStripItem)franceToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)franceToolStripMenuItem).set_Text("France");
			((ToolStripItem)franceToolStripMenuItem).add_Click((EventHandler)franceToolStripMenuItem_Click);
			((ToolStripItem)uSAToolStripMenuItem).set_Image((Image)Resources.FLGUSA02);
			((ToolStripItem)uSAToolStripMenuItem).set_Name("uSAToolStripMenuItem");
			((ToolStripItem)uSAToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)uSAToolStripMenuItem).set_Text("USA");
			((ToolStripItem)uSAToolStripMenuItem).add_Click((EventHandler)uSAToolStripMenuItem_Click);
			((ToolStripItem)canadaToolStripMenuItem).set_Image((Image)Resources.FLGCAN);
			((ToolStripItem)canadaToolStripMenuItem).set_Name("canadaToolStripMenuItem");
			((ToolStripItem)canadaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)canadaToolStripMenuItem).set_Text("Canada");
			((ToolStripItem)canadaToolStripMenuItem).add_Click((EventHandler)canadaToolStripMenuItem_Click);
			((ToolStripItem)japanToolStripMenuItem).set_Image((Image)Resources.FLGJAPAN);
			((ToolStripItem)japanToolStripMenuItem).set_Name("japanToolStripMenuItem");
			((ToolStripItem)japanToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)japanToolStripMenuItem).set_Text("Japan");
			((ToolStripItem)japanToolStripMenuItem).add_Click((EventHandler)japanToolStripMenuItem_Click);
			((ToolStripItem)indiaToolStripMenuItem).set_Image((Image)Resources.FLGIndia);
			((ToolStripItem)indiaToolStripMenuItem).set_Name("indiaToolStripMenuItem");
			((ToolStripItem)indiaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)indiaToolStripMenuItem).set_Text("India");
			((ToolStripItem)indiaToolStripMenuItem).add_Click((EventHandler)indiaToolStripMenuItem_Click);
			((ToolStripItem)chinaToolStripMenuItem).set_Image((Image)Resources.FLGChina);
			((ToolStripItem)chinaToolStripMenuItem).set_Name("chinaToolStripMenuItem");
			((ToolStripItem)chinaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)chinaToolStripMenuItem).set_Text("China");
			((ToolStripItem)chinaToolStripMenuItem).add_Click((EventHandler)chinaToolStripMenuItem_Click);
			((ToolStripItem)uKToolStripMenuItem).set_Image((Image)Resources.FLGUK);
			((ToolStripItem)uKToolStripMenuItem).set_Name("uKToolStripMenuItem");
			((ToolStripItem)uKToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)uKToolStripMenuItem).set_Text("UK");
			((ToolStripItem)uKToolStripMenuItem).add_Click((EventHandler)uKToolStripMenuItem_Click);
			((ToolStripItem)hawaiiToolStripMenuItem).set_Image((Image)Resources.FLGUSA01);
			((ToolStripItem)hawaiiToolStripMenuItem).set_Name("hawaiiToolStripMenuItem");
			((ToolStripItem)hawaiiToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)hawaiiToolStripMenuItem).set_Text("Hawaii");
			((ToolStripItem)hawaiiToolStripMenuItem).add_Click((EventHandler)hawaiiToolStripMenuItem_Click);
			((ToolStripItem)russiaToolStripMenuItem).set_Image((Image)Resources.FLGRUS);
			((ToolStripItem)russiaToolStripMenuItem).set_Name("russiaToolStripMenuItem");
			((ToolStripItem)russiaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)russiaToolStripMenuItem).set_Text("Russia");
			((ToolStripItem)russiaToolStripMenuItem).add_Click((EventHandler)russiaToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(492, 493));
			((Control)this).get_Controls().Add((Control)(object)cmdReturn);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)cmdGetGaiaEDR3);
			((Control)this).get_Controls().Add((Control)(object)label29);
			((Control)this).get_Controls().Add((Control)(object)txtm);
			((Control)this).get_Controls().Add((Control)(object)txtD);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)txtRAs);
			((Control)this).get_Controls().Add((Control)(object)txts);
			((Control)this).get_Controls().Add((Control)(object)txtRAm);
			((Control)this).get_Controls().Add((Control)(object)txtRAh);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)cmdGetU4);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtU4SeqNum);
			((Control)this).get_Controls().Add((Control)(object)txtU4Zone);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("Gaia_EDR3FromVizieR");
			((Control)this).set_Text("Get Gaia EDR3 / DR3) position from VizieR");
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
