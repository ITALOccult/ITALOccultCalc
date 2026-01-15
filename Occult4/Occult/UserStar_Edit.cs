using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class UserStar_Edit : Form
	{
		private readonly string AppPath;

		private IContainer components;

		private TextBox txtRAh;

		private TextBox txtRAm;

		private TextBox txtRAs;

		private TextBox txtpmRA;

		private TextBox txtD;

		private TextBox txtm;

		private TextBox txts;

		private TextBox txtpmDec;

		private TextBox txtMv;

		private TextBox txtMp;

		private TextBox txtID;

		private TextBox txtPi;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Label label13;

		private Label label15;

		private Label label16;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private Label label14;

		private Label label17;

		private Label label18;

		private TextBox txtRV;

		private Label label19;

		private TextBox txtEpoch;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label20;

		private TextBox txtMr;

		private Button cmdGetMp;

		private Button cmdGetMr;

		private Label label21;

		private Panel panel1;

		private Panel panel2;

		private Label label22;

		private Panel panel3;

		private ComboBox cmbDuplicate;

		private Label label25;

		private Label label24;

		private TextBox txtRUWE;

		private Label label23;

		private CheckBox chkPMusingUCAC4;

		private CheckBox chkNoGAIA_PM;

		private ToolTip toolTip1;

		public UserStar_Edit()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void UserStar_Edit_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			string path = AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv";
			if (!File.Exists(path))
			{
				path = AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.dat";
				if (!File.Exists(path))
				{
					return;
				}
			}
			string text;
			using (StreamReader streamReader = new StreamReader(path))
			{
				text = streamReader.ReadLine();
			}
			if (text.Substring(21).Contains(","))
			{
				string[] array = text.Split(new char[1] { ',' });
				((Control)txtID).set_Text(array[0]);
				((Control)txtEpoch).set_Text(array[1]);
				((Control)txtRAh).set_Text(array[2]);
				((Control)txtRAm).set_Text(array[3]);
				((Control)txtRAs).set_Text(array[4]);
				((Control)txtpmRA).set_Text(array[5]);
				((Control)txtD).set_Text(array[6]);
				((Control)txtm).set_Text(array[7]);
				((Control)txts).set_Text(array[8]);
				((Control)txtpmDec).set_Text(array[9]);
				((Control)txtPi).set_Text(array[10]);
				((Control)txtRV).set_Text(array[11]);
				((Control)txtMv).set_Text(array[12]);
				((Control)txtMr).set_Text(array[13]);
				((Control)txtMp).set_Text(array[14]);
				if (array.Length > 15)
				{
					if (array[15].Trim() != "")
					{
						((Control)txtRUWE).set_Text(array[15]);
					}
					else
					{
						((Control)txtRUWE).set_Text("-1");
					}
				}
				if (array.Length > 16)
				{
					int.TryParse(array[16], out var result);
					((ListControl)cmbDuplicate).set_SelectedIndex(result + 1);
				}
				else
				{
					((ListControl)cmbDuplicate).set_SelectedIndex(0);
				}
				if (array.Length > 17)
				{
					chkNoGAIA_PM.set_Checked(array[17].Trim() == "1");
				}
				else
				{
					chkNoGAIA_PM.set_Checked(false);
				}
				if (array.Length > 18)
				{
					chkPMusingUCAC4.set_Checked(array[18].Trim() == "1");
				}
				else
				{
					chkNoGAIA_PM.set_Checked(false);
				}
			}
			else
			{
				((Control)txtID).set_Text(text.Substring(0, 20).Trim());
				((Control)txtEpoch).set_Text("2000.0");
				((Control)txtRAh).set_Text(text.Substring(21, 2));
				((Control)txtRAm).set_Text(text.Substring(23, 2));
				((Control)txtRAs).set_Text(text.Substring(25, 7));
				((Control)txtpmRA).set_Text(text.Substring(32, 9));
				((Control)txtD).set_Text(text.Substring(42, 1) + text.Substring(43, 2).Trim());
				((Control)txtm).set_Text(text.Substring(45, 2));
				((Control)txts).set_Text(text.Substring(47, 6));
				((Control)txtpmDec).set_Text(text.Substring(53, 8));
				((Control)txtPi).set_Text(text.Substring(62, 6));
				((Control)txtRV).set_Text("0");
				((Control)txtMv).set_Text(text.Substring(69, 5));
				((Control)txtMr).set_Text("");
				((Control)txtMp).set_Text(text.Substring(75, 5));
			}
		}

		private void txtID_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtID).SelectAll();
		}

		private void txtRAh_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRAh).SelectAll();
		}

		private void txtRAm_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRAm).SelectAll();
		}

		private void txtRAs_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtRAs).SelectAll();
		}

		private void txtpmRA_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtpmRA).SelectAll();
		}

		private void txtD_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtD).SelectAll();
		}

		private void txtm_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtm).SelectAll();
		}

		private void txts_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txts).SelectAll();
		}

		private void txtpmDec_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtpmDec).SelectAll();
		}

		private void txtPi_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtPi).SelectAll();
		}

		private void txtMv_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMv).SelectAll();
		}

		private void txtMp_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMp).SelectAll();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"User star editor");
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_008d: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c0: Unknown result type (might be due to invalid IL or missing references)
			new StringBuilder();
			if (((Control)txtID).get_Text().Contains(","))
			{
				MessageBox.Show("The star name includes a comma, which is not allowed", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if ((1u & (int.TryParse(((Control)txtRAh).get_Text(), out var result) ? 1u : 0u) & (int.TryParse(((Control)txtRAm).get_Text(), out result) ? 1u : 0u) & (double.TryParse(((Control)txtRAs).get_Text(), out var result2) ? 1u : 0u) & (double.TryParse(((Control)txtpmRA).get_Text(), out result2) ? 1u : 0u)) == 0)
			{
				MessageBox.Show("One or more data fields for RA are blank, or contain non-numeric data", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if ((1u & (int.TryParse(((Control)txtD).get_Text(), out result) ? 1u : 0u) & (int.TryParse(((Control)txtm).get_Text(), out result) ? 1u : 0u) & (double.TryParse(((Control)txts).get_Text(), out result2) ? 1u : 0u) & (double.TryParse(((Control)txtpmDec).get_Text(), out result2) ? 1u : 0u)) == 0)
			{
				MessageBox.Show("One or more data fields for Dec are blank, or contain non-numeric data", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if ((1u & (double.TryParse(((Control)txtEpoch).get_Text(), out result2) ? 1u : 0u) & (double.TryParse(((Control)txtPi).get_Text(), out result2) ? 1u : 0u) & (double.TryParse(((Control)txtRV).get_Text(), out result2) ? 1u : 0u)) == 0)
			{
				MessageBox.Show("Oneo or more of Epoch, Parallax or RV are blank, or contain non-numeric data", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if ((1u & (double.TryParse(((Control)txtMv).get_Text(), out result2) ? 1u : 0u) & ((result2 != 0.0) ? 1u : 0u) & (double.TryParse(((Control)txtMr).get_Text(), out result2) ? 1u : 0u) & ((result2 != 0.0) ? 1u : 0u) & (double.TryParse(((Control)txtMp).get_Text(), out result2) ? 1u : 0u) & ((result2 != 0.0) ? 1u : 0u)) == 0)
			{
				MessageBox.Show("One or more data fields for magnitude are blank, zero, or contain non-numeric data", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\UserStar_MinorPlanetSearch.csv");
			streamWriter.WriteLine(((Control)txtID).get_Text() + "," + ((Control)txtEpoch).get_Text().Trim() + "," + ((Control)txtRAh).get_Text().Trim() + "," + ((Control)txtRAm).get_Text().Trim() + "," + ((Control)txtRAs).get_Text().Trim() + "," + ((Control)txtpmRA).get_Text().Trim() + "," + ((Control)txtD).get_Text().Trim() + "," + ((Control)txtm).get_Text().Trim() + "," + ((Control)txts).get_Text().Trim() + "," + ((Control)txtpmDec).get_Text().Trim() + "," + ((Control)txtPi).get_Text().Trim() + "," + ((Control)txtRV).get_Text().Trim() + "," + ((Control)txtMv).get_Text().Trim() + "," + ((Control)txtMr).get_Text().Trim() + "," + ((Control)txtMp).get_Text().Trim() + "," + ((Control)txtRUWE).get_Text().Trim() + "," + (((ListControl)cmbDuplicate).get_SelectedIndex() - 1) + "," + Convert.ToInt32(chkNoGAIA_PM.get_Checked()) + "," + Convert.ToInt32(chkPMusingUCAC4.get_Checked()));
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void cmdGetMp_Click(object sender, EventArgs e)
		{
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			double.TryParse(((Control)txtMv).get_Text(), out var result);
			double.TryParse(((Control)txtMr).get_Text(), out var result2);
			if (result == 0.0 || result2 == 0.0)
			{
				MessageBox.Show("The calculation reqires both Mv and Mr to be specified", "Insufficient data", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			double num = (1.54 * result - result2) / 0.54;
			((Control)txtMp).set_Text(string.Format("{0,1:f1}", num));
		}

		private void cmdGetMr_Click(object sender, EventArgs e)
		{
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			double.TryParse(((Control)txtMv).get_Text(), out var result);
			double.TryParse(((Control)txtMp).get_Text(), out var result2);
			if (result == 0.0 || result2 == 0.0)
			{
				MessageBox.Show("The calculation reqires both Mv and Mp to be specified", "Insufficient data", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			double num = 1.54 * result - 0.54 * result2;
			((Control)txtMr).set_Text(string.Format("{0,1:f1}", num));
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
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_1ee7: Unknown result type (might be due to invalid IL or missing references)
			//IL_1ef1: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(UserStar_Edit));
			txtRAh = new TextBox();
			txtRAm = new TextBox();
			txtRAs = new TextBox();
			txtpmRA = new TextBox();
			txtD = new TextBox();
			txtm = new TextBox();
			txts = new TextBox();
			txtpmDec = new TextBox();
			txtMv = new TextBox();
			txtMp = new TextBox();
			txtID = new TextBox();
			txtPi = new TextBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			label13 = new Label();
			label15 = new Label();
			label16 = new Label();
			menuStrip1 = new MenuStrip();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label14 = new Label();
			label17 = new Label();
			label18 = new Label();
			txtRV = new TextBox();
			label19 = new Label();
			txtEpoch = new TextBox();
			label20 = new Label();
			txtMr = new TextBox();
			cmdGetMp = new Button();
			cmdGetMr = new Button();
			label21 = new Label();
			panel1 = new Panel();
			panel2 = new Panel();
			label22 = new Label();
			panel3 = new Panel();
			label23 = new Label();
			txtRUWE = new TextBox();
			label24 = new Label();
			label25 = new Label();
			cmbDuplicate = new ComboBox();
			chkNoGAIA_PM = new CheckBox();
			chkPMusingUCAC4 = new CheckBox();
			toolTip1 = new ToolTip(components);
			((Control)menuStrip1).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtRAh).set_Location(new Point(39, 61));
			((Control)txtRAh).set_Name("txtRAh");
			((Control)txtRAh).set_Size(new Size(26, 20));
			((Control)txtRAh).set_TabIndex(3);
			((Control)txtRAh).set_Text("0");
			((Control)txtRAh).add_Enter((EventHandler)txtRAh_Enter);
			((Control)txtRAm).set_Location(new Point(71, 61));
			((Control)txtRAm).set_Name("txtRAm");
			((Control)txtRAm).set_Size(new Size(25, 20));
			((Control)txtRAm).set_TabIndex(4);
			((Control)txtRAm).set_Text("0");
			((Control)txtRAm).add_Enter((EventHandler)txtRAm_Enter);
			((Control)txtRAs).set_Location(new Point(102, 61));
			((Control)txtRAs).set_Name("txtRAs");
			((Control)txtRAs).set_Size(new Size(61, 20));
			((Control)txtRAs).set_TabIndex(5);
			((Control)txtRAs).set_Text("0.00000");
			((Control)txtRAs).add_Enter((EventHandler)txtRAs_Enter);
			((Control)txtpmRA).set_Location(new Point(238, 61));
			((Control)txtpmRA).set_Name("txtpmRA");
			((Control)txtpmRA).set_Size(new Size(59, 20));
			((Control)txtpmRA).set_TabIndex(6);
			((Control)txtpmRA).set_Text("0.000000");
			((Control)txtpmRA).add_Enter((EventHandler)txtpmRA_Enter);
			((Control)txtD).set_ImeMode((ImeMode)0);
			((Control)txtD).set_Location(new Point(39, 96));
			((Control)txtD).set_Name("txtD");
			((Control)txtD).set_Size(new Size(26, 20));
			((Control)txtD).set_TabIndex(7);
			((Control)txtD).set_Text("0");
			((Control)txtD).add_Enter((EventHandler)txtD_Enter);
			((Control)txtm).set_Location(new Point(71, 96));
			((Control)txtm).set_Name("txtm");
			((Control)txtm).set_Size(new Size(25, 20));
			((Control)txtm).set_TabIndex(8);
			((Control)txtm).set_Text("0");
			((Control)txtm).add_Enter((EventHandler)txtm_Enter);
			((Control)txts).set_Location(new Point(102, 96));
			((Control)txts).set_Name("txts");
			((Control)txts).set_Size(new Size(48, 20));
			((Control)txts).set_TabIndex(9);
			((Control)txts).set_Text("0.0000");
			((Control)txts).add_Enter((EventHandler)txts_Enter);
			((Control)txtpmDec).set_Location(new Point(238, 96));
			((Control)txtpmDec).set_Name("txtpmDec");
			((Control)txtpmDec).set_Size(new Size(49, 20));
			((Control)txtpmDec).set_TabIndex(10);
			((Control)txtpmDec).set_Text("0.00000");
			((Control)txtpmDec).add_Enter((EventHandler)txtpmDec_Enter);
			((Control)txtMv).set_Location(new Point(146, 23));
			((Control)txtMv).set_Name("txtMv");
			((Control)txtMv).set_Size(new Size(37, 20));
			((Control)txtMv).set_TabIndex(4);
			((Control)txtMv).set_Text("12.0");
			((Control)txtMv).add_Enter((EventHandler)txtMv_Enter);
			((Control)txtMp).set_Location(new Point(255, 23));
			((Control)txtMp).set_Name("txtMp");
			((Control)txtMp).set_Size(new Size(37, 20));
			((Control)txtMp).set_TabIndex(6);
			((Control)txtMp).set_Text("12.0");
			((Control)txtMp).add_Enter((EventHandler)txtMp_Enter);
			((Control)txtID).set_Location(new Point(142, 36));
			((TextBoxBase)txtID).set_MaxLength(21);
			((Control)txtID).set_Name("txtID");
			((Control)txtID).set_Size(new Size(130, 20));
			((Control)txtID).set_TabIndex(1);
			((Control)txtID).set_Text("X");
			((Control)txtID).add_Enter((EventHandler)txtID_Enter);
			((Control)txtPi).set_Location(new Point(95, 132));
			((Control)txtPi).set_Name("txtPi");
			((Control)txtPi).set_Size(new Size(50, 20));
			((Control)txtPi).set_TabIndex(11);
			((Control)txtPi).set_Text("0.0000");
			((Control)txtPi).add_Enter((EventHandler)txtPi_Enter);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(55, 40));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(83, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Star identifier");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(11, 65));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(24, 13));
			((Control)label2).set_TabIndex(13);
			((Control)label2).set_Text("RA");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(6, 100));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(30, 13));
			((Control)label3).set_TabIndex(19);
			((Control)label3).set_Text("Dec");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(42, 136));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(52, 13));
			((Control)label4).set_TabIndex(24);
			((Control)label4).set_Text("Parallax");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(103, 27));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(41, 13));
			((Control)label5).set_TabIndex(3);
			((Control)label5).set_Text("Visual");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(221, 27));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(32, 13));
			((Control)label6).set_TabIndex(5);
			((Control)label6).set_Text("Blue");
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(186, 68));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(52, 40));
			((Control)label7).set_TabIndex(17);
			((Control)label7).set_Text("Annual\r\nproper\r\nmotions");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(47, 47));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(13, 13));
			((Control)label8).set_TabIndex(14);
			((Control)label8).set_Text("h");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(76, 47));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(15, 13));
			((Control)label9).set_TabIndex(15);
			((Control)label9).set_Text("m");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(118, 47));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(12, 13));
			((Control)label10).set_TabIndex(16);
			((Control)label10).set_Text("s");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(253, 47));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(12, 13));
			((Control)label11).set_TabIndex(18);
			((Control)label11).set_Text("s");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(47, 84));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(13, 13));
			((Control)label12).set_TabIndex(20);
			((Control)label12).set_Text("o");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(76, 89));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(9, 13));
			((Control)label13).set_TabIndex(21);
			((Control)label13).set_Text("'");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(253, 88));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(12, 13));
			((Control)label15).set_TabIndex(23);
			((Control)label15).set_Text("\"");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(111, 89));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(12, 13));
			((Control)label16).set_TabIndex(22);
			((Control)label16).set_Text("\"");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(327, 24));
			((Control)menuStrip1).set_TabIndex(28);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(86, 20));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save         ");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(84, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help        ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(100, 123));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(12, 13));
			((Control)label14).set_TabIndex(25);
			((Control)label14).set_Text("\"");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(198, 119));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(43, 13));
			((Control)label17).set_TabIndex(27);
			((Control)label17).set_Text("km/sec");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(163, 137));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(24, 13));
			((Control)label18).set_TabIndex(26);
			((Control)label18).set_Text("RV");
			((Control)txtRV).set_Location(new Point(190, 133));
			((Control)txtRV).set_Name("txtRV");
			((Control)txtRV).set_Size(new Size(50, 20));
			((Control)txtRV).set_TabIndex(12);
			((Control)txtRV).set_Text("0.00");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(101, 29));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(43, 13));
			((Control)label19).set_TabIndex(1);
			((Control)label19).set_Text("Epoch");
			((Control)txtEpoch).set_Location(new Point(151, 25));
			((Control)txtEpoch).set_Name("txtEpoch");
			((Control)txtEpoch).set_Size(new Size(50, 20));
			((Control)txtEpoch).set_TabIndex(2);
			((Control)txtEpoch).set_Text("2000.0");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(5, 27));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(30, 13));
			((Control)label20).set_TabIndex(1);
			((Control)label20).set_Text("Red");
			((Control)txtMr).set_Location(new Point(37, 23));
			((Control)txtMr).set_Name("txtMr");
			((Control)txtMr).set_Size(new Size(37, 20));
			((Control)txtMr).set_TabIndex(2);
			((Control)txtMr).set_Text("12.0");
			((Control)cmdGetMp).set_BackColor(Color.PaleTurquoise);
			((ButtonBase)cmdGetMp).get_FlatAppearance().set_BorderColor(Color.Black);
			((ButtonBase)cmdGetMp).set_FlatStyle((FlatStyle)0);
			((Control)cmdGetMp).set_Location(new Point(30, 50));
			((Control)cmdGetMp).set_Name("cmdGetMp");
			((Control)cmdGetMp).set_Size(new Size(236, 24));
			((Control)cmdGetMp).set_TabIndex(7);
			((Control)cmdGetMp).set_Text("Estimate the Mp magnitude using Mv and Mr");
			((ButtonBase)cmdGetMp).set_UseVisualStyleBackColor(false);
			((Control)cmdGetMp).add_Click((EventHandler)cmdGetMp_Click);
			((Control)cmdGetMr).set_BackColor(Color.MistyRose);
			((ButtonBase)cmdGetMr).get_FlatAppearance().set_BorderColor(Color.Black);
			((ButtonBase)cmdGetMr).set_FlatStyle((FlatStyle)0);
			((Control)cmdGetMr).set_Location(new Point(30, 77));
			((Control)cmdGetMr).set_Name("cmdGetMr");
			((Control)cmdGetMr).set_Size(new Size(236, 24));
			((Control)cmdGetMr).set_TabIndex(8);
			((Control)cmdGetMr).set_Text("Estimate the Mr magnitude using Mv and Mp");
			((ButtonBase)cmdGetMr).set_UseVisualStyleBackColor(false);
			((Control)cmdGetMr).add_Click((EventHandler)cmdGetMr_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(107, 3));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(82, 15));
			((Control)label21).set_TabIndex(0);
			((Control)label21).set_Text("Magnitudes");
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label21);
			((Control)panel1).get_Controls().Add((Control)(object)cmdGetMr);
			((Control)panel1).get_Controls().Add((Control)(object)cmdGetMp);
			((Control)panel1).get_Controls().Add((Control)(object)label20);
			((Control)panel1).get_Controls().Add((Control)(object)txtMr);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)txtMp);
			((Control)panel1).get_Controls().Add((Control)(object)txtMv);
			((Control)panel1).set_Location(new Point(15, 233));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(303, 114));
			((Control)panel1).set_TabIndex(3);
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)label22);
			((Control)panel2).get_Controls().Add((Control)(object)label19);
			((Control)panel2).get_Controls().Add((Control)(object)txtEpoch);
			((Control)panel2).get_Controls().Add((Control)(object)label17);
			((Control)panel2).get_Controls().Add((Control)(object)label18);
			((Control)panel2).get_Controls().Add((Control)(object)txtRV);
			((Control)panel2).get_Controls().Add((Control)(object)label11);
			((Control)panel2).get_Controls().Add((Control)(object)label10);
			((Control)panel2).get_Controls().Add((Control)(object)label9);
			((Control)panel2).get_Controls().Add((Control)(object)label8);
			((Control)panel2).get_Controls().Add((Control)(object)label7);
			((Control)panel2).get_Controls().Add((Control)(object)label4);
			((Control)panel2).get_Controls().Add((Control)(object)label3);
			((Control)panel2).get_Controls().Add((Control)(object)label2);
			((Control)panel2).get_Controls().Add((Control)(object)txtPi);
			((Control)panel2).get_Controls().Add((Control)(object)txtpmDec);
			((Control)panel2).get_Controls().Add((Control)(object)txts);
			((Control)panel2).get_Controls().Add((Control)(object)txtm);
			((Control)panel2).get_Controls().Add((Control)(object)txtD);
			((Control)panel2).get_Controls().Add((Control)(object)txtpmRA);
			((Control)panel2).get_Controls().Add((Control)(object)txtRAs);
			((Control)panel2).get_Controls().Add((Control)(object)txtRAm);
			((Control)panel2).get_Controls().Add((Control)(object)txtRAh);
			((Control)panel2).get_Controls().Add((Control)(object)label15);
			((Control)panel2).get_Controls().Add((Control)(object)label16);
			((Control)panel2).get_Controls().Add((Control)(object)label13);
			((Control)panel2).get_Controls().Add((Control)(object)label12);
			((Control)panel2).get_Controls().Add((Control)(object)label14);
			((Control)panel2).set_Location(new Point(15, 68));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(303, 159));
			((Control)panel2).set_TabIndex(2);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(121, 3));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(59, 15));
			((Control)label22).set_TabIndex(0);
			((Control)label22).set_Text("Position");
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)chkPMusingUCAC4);
			((Control)panel3).get_Controls().Add((Control)(object)chkNoGAIA_PM);
			((Control)panel3).get_Controls().Add((Control)(object)cmbDuplicate);
			((Control)panel3).get_Controls().Add((Control)(object)label25);
			((Control)panel3).get_Controls().Add((Control)(object)label24);
			((Control)panel3).get_Controls().Add((Control)(object)txtRUWE);
			((Control)panel3).get_Controls().Add((Control)(object)label23);
			((Control)panel3).set_Location(new Point(15, 358));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(303, 94));
			((Control)panel3).set_TabIndex(4);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(111, 2));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(71, 15));
			((Control)label23).set_TabIndex(0);
			((Control)label23).set_Text("Reliability");
			((Control)txtRUWE).set_Location(new Point(73, 29));
			((Control)txtRUWE).set_Name("txtRUWE");
			((Control)txtRUWE).set_Size(new Size(40, 20));
			((Control)txtRUWE).set_TabIndex(2);
			((Control)txtRUWE).set_Text("-1");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(130, 26));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(65, 26));
			((Control)label24).set_TabIndex(3);
			((Control)label24).set_Text("Duplicate \r\nSource");
			label24.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(28, 33));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(45, 13));
			((Control)label25).set_TabIndex(1);
			((Control)label25).set_Text("RUWE");
			label25.set_TextAlign(ContentAlignment.MiddleCenter);
			((ListControl)cmbDuplicate).set_FormattingEnabled(true);
			cmbDuplicate.get_Items().AddRange(new object[3] { "Don't know", "No", "Yes" });
			((Control)cmbDuplicate).set_Location(new Point(195, 29));
			((Control)cmbDuplicate).set_Name("cmbDuplicate");
			((Control)cmbDuplicate).set_Size(new Size(86, 21));
			((Control)cmbDuplicate).set_TabIndex(4);
			((Control)cmbDuplicate).set_Text("Don't know");
			((Control)chkNoGAIA_PM).set_AutoSize(true);
			chkNoGAIA_PM.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkNoGAIA_PM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkNoGAIA_PM).set_Location(new Point(6, 59));
			((Control)chkNoGAIA_PM).set_Name("chkNoGAIA_PM");
			((Control)chkNoGAIA_PM).set_Size(new Size(107, 30));
			((Control)chkNoGAIA_PM).set_TabIndex(5);
			((Control)chkNoGAIA_PM).set_Text("No proper \r\nmotion in Gaia");
			toolTip1.SetToolTip((Control)(object)chkNoGAIA_PM, "Relates to the status in Gaia. If proper \r\nmotion  has been derived using UCAC4\r\n as the early epoch, still check this.");
			((ButtonBase)chkNoGAIA_PM).set_UseVisualStyleBackColor(true);
			((Control)chkPMusingUCAC4).set_AutoSize(true);
			chkPMusingUCAC4.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkPMusingUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkPMusingUCAC4).set_Location(new Point(159, 59));
			((Control)chkPMusingUCAC4).set_Name("chkPMusingUCAC4");
			((Control)chkPMusingUCAC4).set_Size(new Size(122, 30));
			((Control)chkPMusingUCAC4).set_TabIndex(6);
			((Control)chkPMusingUCAC4).set_Text("Proper motion \r\nbased on UCAC4");
			toolTip1.SetToolTip((Control)(object)chkPMusingUCAC4, "Check when the proper motion has\r\nbeen derived using UCAC4. This\r\ncheck is additional to the No proper\r\nmotion in Gaia check.");
			((ButtonBase)chkPMusingUCAC4).set_UseVisualStyleBackColor(true);
			toolTip1.set_IsBalloon(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(327, 456));
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtID);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterUserStar", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationAsterUserStar);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("UserStar_Edit");
			((Control)this).set_Text("User Star edit");
			((Form)this).add_Load((EventHandler)UserStar_Edit_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
