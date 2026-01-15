using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class CreateUserCatalogue : Form
	{
		private const double Radian = 180.0 / Math.PI;

		private string SourceFile = "";

		private IContainer components;

		private RadioButton optA;

		private RadioButton optF;

		private RadioButton optE;

		private RadioButton optD;

		private RadioButton optC;

		private RadioButton optB;

		private GroupBox groupBox2;

		private Button cmdSelectFile;

		private ListBox lstFormat;

		private GroupBox groupBox3;

		private Label label4;

		private Label label3;

		private Label label2;

		private Label label1;

		private Label label18;

		private Label label17;

		private Label label15;

		private NumericUpDown updnDECSec;

		private NumericUpDown updnRASecs;

		private Label label11;

		private Label label12;

		private NumericUpDown updnDECMin;

		private NumericUpDown updnRAMins;

		private Label label5;

		private Label label6;

		private NumericUpDown updnDECdeg;

		private NumericUpDown updnRAhrs;

		private Label label19;

		private NumericUpDown updnMRCols;

		private Label label20;

		private NumericUpDown updnMR;

		private Label label22;

		private Label label8;

		private NumericUpDown updnMVCols;

		private Label label9;

		private NumericUpDown updnMV;

		private Label label10;

		private Label label7;

		private CheckBox chkDECSecHasDecimal;

		private CheckBox chkRAsecHasDecimal;

		private Label label21;

		private NumericUpDown updnDECSecsLength;

		private NumericUpDown updnRASecsLength;

		private ListBox lstTest;

		private Button cmdTestRead;

		private Label label23;

		private NumericUpDown updnIdentifierCols;

		private Label label24;

		private NumericUpDown updnIdentifier;

		private Label label26;

		private GroupBox groupBox4;

		private Button cmdCreateUser;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private CheckBox chkPrecess;

		public CreateUserCatalogue()
		{
			InitializeComponent();
		}

		private void CreateUserCatalogue_Load(object sender, EventArgs e)
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
		}

		private void cmdSelectFile_Click(object sender, EventArgs e)
		{
			//IL_00c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00df: Invalid comparison between Unknown and I4
			string text = "";
			string text2 = "";
			string text3 = "";
			string text4 = "";
			string text5 = "";
			int num = 0;
			for (int i = 1; i < 4680; i++)
			{
				text5 = string.Format("{0,4:f0}", i);
				num++;
				if (num >= 10)
				{
					text += text5.Substring(0, 1);
					text2 += text5.Substring(1, 1);
					text3 += text5.Substring(2, 1);
					num = 0;
				}
				else
				{
					text += " ";
					text2 += " ";
					text3 += " ";
				}
				text4 += text5.Substring(3, 1);
			}
			OpenFileDialog val = new OpenFileDialog();
			((FileDialog)val).set_Title("Select source file for catalogue");
			if ((int)((CommonDialog)val).ShowDialog() != 1)
			{
				return;
			}
			SourceFile = ((FileDialog)val).get_FileName();
			using StreamReader streamReader = new StreamReader(SourceFile);
			lstFormat.get_Items().Clear();
			lstFormat.get_Items().Add((object)text3);
			lstFormat.get_Items().Add((object)text4);
			try
			{
				lstFormat.get_Items().Add((object)streamReader.ReadLine());
				lstFormat.get_Items().Add((object)streamReader.ReadLine());
				lstFormat.get_Items().Add((object)streamReader.ReadLine());
				lstFormat.get_Items().Add((object)streamReader.ReadLine());
			}
			catch
			{
			}
			lstFormat.set_HorizontalExtent(7 * lstFormat.get_Items().get_Item(3).ToString()!.Length + 40);
		}

		private void cmdTestRead_Click(object sender, EventArgs e)
		{
			using StreamReader streamReader = new StreamReader(SourceFile);
			lstTest.get_Items().Clear();
			lstTest.get_Items().Add((object)"hh mm ss.sss   -dd mm ss.ss    vv.vvv  rr.rrr  nnnnnnnn");
			try
			{
				lstTest.get_Items().Add((object)ReformattedCatalogueLine(streamReader.ReadLine()!.PadRight(200)));
				lstTest.get_Items().Add((object)ReformattedCatalogueLine(streamReader.ReadLine()!.PadRight(200)));
				lstTest.get_Items().Add((object)ReformattedCatalogueLine(streamReader.ReadLine()!.PadRight(200)));
				lstTest.get_Items().Add((object)ReformattedCatalogueLine(streamReader.ReadLine()!.PadRight(200)));
			}
			catch
			{
			}
		}

		private string ReformattedCatalogueLine(string X)
		{
			StringBuilder stringBuilder = new StringBuilder();
			string text;
			if (chkPrecess.get_Checked())
			{
				if (!double.TryParse(X.Substring((int)updnRAhrs.get_Value() - 1, 2), out var result))
				{
					result = 0.0;
				}
				if (!double.TryParse(X.Substring((int)updnRAMins.get_Value() - 1, 2), out var result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(X.Substring((int)updnRASecs.get_Value() - 1, (int)updnRASecsLength.get_Value()), out var result3))
				{
					result3 = 0.0;
				}
				bool num = X.Substring((int)updnDECdeg.get_Value() - 1, 2).Contains("-");
				if (!double.TryParse(X.Substring((int)updnDECdeg.get_Value() - 1, 3).Replace("-", " ").Replace("+", " "), out var result4))
				{
					result4 = 0.0;
				}
				if (!double.TryParse(X.Substring((int)updnDECMin.get_Value() - 1, 2), out var result5))
				{
					result5 = 0.0;
				}
				if (!double.TryParse(X.Substring((int)updnDECSec.get_Value() - 1, (int)updnDECSecsLength.get_Value()), out var result6))
				{
					result6 = 0.0;
				}
				double RA = (result + result2 / 60.0 + result3 / 3600.0) * 15.0 / (180.0 / Math.PI);
				double Dec = (result4 + result5 / 60.0 + result6 / 3600.0) / (180.0 / Math.PI);
				if (num)
				{
					Dec = 0.0 - Dec;
				}
				Utilities.Precession(1950, 2451545.0, use2006values_Not1976: false, ref RA, ref Dec, 0.0, 0.0);
				int num2 = (int)updnRASecsLength.get_Value() - 3;
				if (num2 < 0)
				{
					num2 = 0;
				}
				stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, num2, MinutesOnly: false).PadRight(15));
				num2 = (int)updnDECSecsLength.get_Value() - 3;
				if (num2 < 0)
				{
					num2 = 0;
				}
				stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, num2, MinutesOnly: false).PadRight(14));
			}
			else
			{
				stringBuilder.Append(X.Substring((int)updnRAhrs.get_Value() - 1, 2).Trim().PadLeft(2, '0'));
				stringBuilder.Append(" " + X.Substring((int)updnRAMins.get_Value() - 1, 2).Trim().PadLeft(2, '0'));
				text = X.Substring((int)updnRASecs.get_Value() - 1, (int)updnRASecsLength.get_Value());
				if (!chkRAsecHasDecimal.get_Checked())
				{
					text = text.Insert(2, ".");
				}
				if (text.Substring(0, 1) == " ")
				{
					text = "0" + text.Substring(1);
				}
				stringBuilder.Append(" " + text.PadRight(8));
				text = X.Substring((int)updnDECdeg.get_Value() - 1, 3);
				if (text.Contains("-"))
				{
					stringBuilder.Append(" -");
					text = text.Replace("-", " ");
				}
				else if (text.Contains("+"))
				{
					stringBuilder.Append(" +");
					text = text.Replace("+", " ");
				}
				else
				{
					stringBuilder.Append(" +");
				}
				stringBuilder.Append(text.Trim().PadLeft(2, '0'));
				stringBuilder.Append(" " + X.Substring((int)updnDECMin.get_Value() - 1, 2).Trim().PadLeft(2, '0'));
				text = X.Substring((int)updnDECSec.get_Value() - 1, (int)updnDECSecsLength.get_Value());
				if (!chkDECSecHasDecimal.get_Checked())
				{
					text = text.Insert(2, ".");
				}
				if (text.Substring(0, 1) == " ")
				{
					text = "0" + text.Substring(1);
				}
				stringBuilder.Append(" " + text.PadRight(7));
			}
			text = X.Substring((int)updnMV.get_Value() - 1, (int)updnMVCols.get_Value());
			if (text.IndexOf(".") < 0)
			{
				stringBuilder.Append("".PadRight(8));
			}
			else
			{
				while (text.IndexOf(".") < 2)
				{
					text = " " + text;
				}
				while (text.IndexOf(".") > 2)
				{
					text = text.Substring(1);
				}
				stringBuilder.Append("  " + text.PadRight(6).Substring(0, 6));
			}
			text = X.Substring((int)updnMR.get_Value() - 1, (int)updnMRCols.get_Value());
			if (text.IndexOf(".") < 0)
			{
				stringBuilder.Append("".PadRight(8));
			}
			else
			{
				while (text.IndexOf(".") < 2)
				{
					text = " " + text;
				}
				while (text.IndexOf(".") > 2)
				{
					text = text.Substring(1);
				}
				stringBuilder.Append("  " + text.PadRight(6).Substring(0, 6));
			}
			stringBuilder.Append("  " + X.Substring((int)updnIdentifier.get_Value() - 1, (int)updnIdentifierCols.get_Value()).PadRight(9).Substring(0, 9));
			return stringBuilder.ToString();
		}

		private void cmdCreateUser_Click(object sender, EventArgs e)
		{
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c7: Invalid comparison between Unknown and I4
			ArrayList arrayList = new ArrayList();
			int num = 1;
			byte value = 7;
			string text = (optA.get_Checked() ? "UserA" : (optB.get_Checked() ? "UserB" : (optC.get_Checked() ? "UserC" : (optD.get_Checked() ? "UserD" : ((!optE.get_Checked()) ? "UserF" : "UserE")))));
			if (File.Exists(Utilities.AppPath + "\\Resource Files\\" + text + ".bin") && (int)MessageBox.Show("User catalogue " + text + " exists\r\n\r\nDo you want to overwrite it?", "File exists", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(SourceFile))
			{
				do
				{
					string text2 = streamReader.ReadLine()!.PadRight(200);
					if (text2.Trim().Length > 10)
					{
						arrayList.Add(ReformattedCatalogueLine(text2));
					}
				}
				while (!streamReader.EndOfStream);
			}
			arrayList.Sort();
			BinaryWriter binaryWriter = new BinaryWriter(new FileStream(Utilities.AppPath + "\\Resource Files\\" + text + ".bin", FileMode.Create, FileAccess.Write));
			BinaryWriter binaryWriter2 = new BinaryWriter(new FileStream(Utilities.AppPath + "\\Resource Files\\" + text + ".inx", FileMode.Create, FileAccess.Write));
			for (int num2 = 89; num2 >= -90; num2--)
			{
				int i = 0;
				string text3 = ((num2 < 0) ? ("-" + (-num2 - 1).ToString().PadLeft(2, '0')) : ("+" + num2.ToString().PadLeft(2, '0')));
				float num3 = 0f;
				short num4 = 0;
				for (int j = 0; j < arrayList.Count; j++)
				{
					string text4 = arrayList[j]!.ToString();
					if (text4.Substring(15, 3) == text3)
					{
						double num5 = 15.0 * (double.Parse(text4.Substring(0, 2)) + double.Parse(text4.Substring(3, 2)) / 60.0 + double.Parse(text4.Substring(6, 7)) / 3600.0);
						double num6 = double.Parse(text4.Substring(15, 3).Replace("-", " ").Replace("+", " ")) + double.Parse(text4.Substring(19, 2)) / 60.0 + double.Parse(text4.Substring(22, 6)) / 3600.0;
						if (text4.Substring(15, 3).Contains("-"))
						{
							num6 = 0.0 - num6;
						}
						for (; num5 > (double)i && i <= 360; i++)
						{
							binaryWriter2.Write(num);
						}
						num++;
						int value2 = (int)(10000000.0 * (num5 - 180.0));
						binaryWriter.Write(value2);
						num3 = 0f;
						binaryWriter.Write(num3);
						value2 = (int)(10000000.0 * num6);
						binaryWriter.Write(value2);
						num3 = 0f;
						binaryWriter.Write(num3);
						num4 = 0;
						binaryWriter.Write(num4);
						if (!float.TryParse(text4.Substring(31, 5), out num3))
						{
							num3 = 90f;
						}
						num4 = Convert.ToInt16(100f * num3);
						binaryWriter.Write(num4);
						if (!float.TryParse(text4.Substring(39, 5), out num3))
						{
							num3 = 90f;
						}
						num4 = Convert.ToInt16(100f * num3);
						binaryWriter.Write(num4);
						binaryWriter.Write(value);
						if (!int.TryParse(text4.Substring(47, 7), out value2))
						{
							value2 = 0;
						}
						binaryWriter.Write(value2);
					}
				}
				for (; i <= 360; i++)
				{
					binaryWriter2.Write(num);
				}
			}
			binaryWriter.Close();
			binaryWriter2.Close();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Create User Catalogue");
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
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0e92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e9c: Expected O, but got Unknown
			//IL_0fe8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ff2: Expected O, but got Unknown
			//IL_11c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_11cc: Expected O, but got Unknown
			//IL_12fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_1307: Expected O, but got Unknown
			//IL_14d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_14e1: Expected O, but got Unknown
			//IL_1612: Unknown result type (might be due to invalid IL or missing references)
			//IL_161c: Expected O, but got Unknown
			//IL_1813: Unknown result type (might be due to invalid IL or missing references)
			//IL_181d: Expected O, but got Unknown
			//IL_18ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_18f4: Expected O, but got Unknown
			//IL_1a15: Unknown result type (might be due to invalid IL or missing references)
			//IL_1a1f: Expected O, but got Unknown
			//IL_1ae9: Unknown result type (might be due to invalid IL or missing references)
			//IL_1af3: Expected O, but got Unknown
			//IL_1d3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1d47: Expected O, but got Unknown
			//IL_1e15: Unknown result type (might be due to invalid IL or missing references)
			//IL_1e1f: Expected O, but got Unknown
			//IL_1fee: Unknown result type (might be due to invalid IL or missing references)
			//IL_1ff8: Expected O, but got Unknown
			//IL_20c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_20d0: Expected O, but got Unknown
			//IL_229e: Unknown result type (might be due to invalid IL or missing references)
			//IL_22a8: Expected O, but got Unknown
			//IL_2376: Unknown result type (might be due to invalid IL or missing references)
			//IL_2380: Expected O, but got Unknown
			//IL_2a26: Unknown result type (might be due to invalid IL or missing references)
			//IL_2a30: Expected O, but got Unknown
			optA = new RadioButton();
			optF = new RadioButton();
			optE = new RadioButton();
			optD = new RadioButton();
			optC = new RadioButton();
			optB = new RadioButton();
			groupBox2 = new GroupBox();
			lstFormat = new ListBox();
			cmdSelectFile = new Button();
			groupBox3 = new GroupBox();
			lstTest = new ListBox();
			cmdTestRead = new Button();
			label23 = new Label();
			updnIdentifierCols = new NumericUpDown();
			label24 = new Label();
			updnIdentifier = new NumericUpDown();
			label26 = new Label();
			label19 = new Label();
			updnMRCols = new NumericUpDown();
			label20 = new Label();
			updnMR = new NumericUpDown();
			label22 = new Label();
			label8 = new Label();
			updnMVCols = new NumericUpDown();
			label9 = new Label();
			updnMV = new NumericUpDown();
			label10 = new Label();
			label7 = new Label();
			chkDECSecHasDecimal = new CheckBox();
			chkRAsecHasDecimal = new CheckBox();
			label21 = new Label();
			updnDECSecsLength = new NumericUpDown();
			updnRASecsLength = new NumericUpDown();
			label18 = new Label();
			label17 = new Label();
			label15 = new Label();
			updnDECSec = new NumericUpDown();
			updnRASecs = new NumericUpDown();
			label11 = new Label();
			label12 = new Label();
			updnDECMin = new NumericUpDown();
			updnRAMins = new NumericUpDown();
			label5 = new Label();
			label6 = new Label();
			updnDECdeg = new NumericUpDown();
			updnRAhrs = new NumericUpDown();
			label4 = new Label();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			groupBox4 = new GroupBox();
			cmdCreateUser = new Button();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			chkPrecess = new CheckBox();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((ISupportInitialize)updnIdentifierCols).BeginInit();
			((ISupportInitialize)updnIdentifier).BeginInit();
			((ISupportInitialize)updnMRCols).BeginInit();
			((ISupportInitialize)updnMR).BeginInit();
			((ISupportInitialize)updnMVCols).BeginInit();
			((ISupportInitialize)updnMV).BeginInit();
			((ISupportInitialize)updnDECSecsLength).BeginInit();
			((ISupportInitialize)updnRASecsLength).BeginInit();
			((ISupportInitialize)updnDECSec).BeginInit();
			((ISupportInitialize)updnRASecs).BeginInit();
			((ISupportInitialize)updnDECMin).BeginInit();
			((ISupportInitialize)updnRAMins).BeginInit();
			((ISupportInitialize)updnDECdeg).BeginInit();
			((ISupportInitialize)updnRAhrs).BeginInit();
			((Control)groupBox4).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)optA).set_AutoSize(true);
			optA.set_Checked(true);
			((Control)optA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optA).set_Location(new Point(28, 33));
			((Control)optA).set_Name("optA");
			((Control)optA).set_Size(new Size(54, 17));
			((Control)optA).set_TabIndex(0);
			optA.set_TabStop(true);
			((Control)optA).set_Text("UserA");
			((ButtonBase)optA).set_UseVisualStyleBackColor(true);
			((Control)optF).set_AutoSize(true);
			((Control)optF).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optF).set_Location(new Point(216, 66));
			((Control)optF).set_Name("optF");
			((Control)optF).set_Size(new Size(53, 17));
			((Control)optF).set_TabIndex(5);
			optF.set_TabStop(true);
			((Control)optF).set_Text("UserF");
			((ButtonBase)optF).set_UseVisualStyleBackColor(true);
			((Control)optE).set_AutoSize(true);
			((Control)optE).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optE).set_Location(new Point(216, 33));
			((Control)optE).set_Name("optE");
			((Control)optE).set_Size(new Size(54, 17));
			((Control)optE).set_TabIndex(4);
			optE.set_TabStop(true);
			((Control)optE).set_Text("UserE");
			((ButtonBase)optE).set_UseVisualStyleBackColor(true);
			((Control)optD).set_AutoSize(true);
			((Control)optD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optD).set_Location(new Point(122, 66));
			((Control)optD).set_Name("optD");
			((Control)optD).set_Size(new Size(55, 17));
			((Control)optD).set_TabIndex(3);
			optD.set_TabStop(true);
			((Control)optD).set_Text("UserD");
			((ButtonBase)optD).set_UseVisualStyleBackColor(true);
			((Control)optC).set_AutoSize(true);
			((Control)optC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optC).set_Location(new Point(122, 33));
			((Control)optC).set_Name("optC");
			((Control)optC).set_Size(new Size(54, 17));
			((Control)optC).set_TabIndex(2);
			optC.set_TabStop(true);
			((Control)optC).set_Text("UserC");
			((ButtonBase)optC).set_UseVisualStyleBackColor(true);
			((Control)optB).set_AutoSize(true);
			((Control)optB).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optB).set_Location(new Point(28, 66));
			((Control)optB).set_Name("optB");
			((Control)optB).set_Size(new Size(54, 17));
			((Control)optB).set_TabIndex(1);
			optB.set_TabStop(true);
			((Control)optB).set_Text("UserB");
			((ButtonBase)optB).set_UseVisualStyleBackColor(true);
			((Control)groupBox2).get_Controls().Add((Control)(object)lstFormat);
			((Control)groupBox2).get_Controls().Add((Control)(object)cmdSelectFile);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(7, 36));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(825, 128));
			((Control)groupBox2).set_TabIndex(0);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Select input file");
			((Control)lstFormat).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstFormat).set_FormattingEnabled(true);
			lstFormat.set_HorizontalExtent(1800);
			lstFormat.set_HorizontalScrollbar(true);
			lstFormat.set_ItemHeight(14);
			((Control)lstFormat).set_Location(new Point(122, 15));
			((Control)lstFormat).set_Name("lstFormat");
			lstFormat.set_ScrollAlwaysVisible(true);
			((Control)lstFormat).set_Size(new Size(692, 102));
			((Control)lstFormat).set_TabIndex(1);
			((Control)cmdSelectFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdSelectFile).set_Location(new Point(6, 54));
			((Control)cmdSelectFile).set_Name("cmdSelectFile");
			((Control)cmdSelectFile).set_Size(new Size(103, 25));
			((Control)cmdSelectFile).set_TabIndex(0);
			((Control)cmdSelectFile).set_Text("Select source file");
			((ButtonBase)cmdSelectFile).set_UseVisualStyleBackColor(true);
			((Control)cmdSelectFile).add_Click((EventHandler)cmdSelectFile_Click);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkPrecess);
			((Control)groupBox3).get_Controls().Add((Control)(object)lstTest);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdTestRead);
			((Control)groupBox3).get_Controls().Add((Control)(object)label23);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnIdentifierCols);
			((Control)groupBox3).get_Controls().Add((Control)(object)label24);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnIdentifier);
			((Control)groupBox3).get_Controls().Add((Control)(object)label26);
			((Control)groupBox3).get_Controls().Add((Control)(object)label19);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnMRCols);
			((Control)groupBox3).get_Controls().Add((Control)(object)label20);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnMR);
			((Control)groupBox3).get_Controls().Add((Control)(object)label22);
			((Control)groupBox3).get_Controls().Add((Control)(object)label8);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnMVCols);
			((Control)groupBox3).get_Controls().Add((Control)(object)label9);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnMV);
			((Control)groupBox3).get_Controls().Add((Control)(object)label10);
			((Control)groupBox3).get_Controls().Add((Control)(object)label7);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkDECSecHasDecimal);
			((Control)groupBox3).get_Controls().Add((Control)(object)chkRAsecHasDecimal);
			((Control)groupBox3).get_Controls().Add((Control)(object)label21);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnDECSecsLength);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnRASecsLength);
			((Control)groupBox3).get_Controls().Add((Control)(object)label18);
			((Control)groupBox3).get_Controls().Add((Control)(object)label17);
			((Control)groupBox3).get_Controls().Add((Control)(object)label15);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnDECSec);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnRASecs);
			((Control)groupBox3).get_Controls().Add((Control)(object)label11);
			((Control)groupBox3).get_Controls().Add((Control)(object)label12);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnDECMin);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnRAMins);
			((Control)groupBox3).get_Controls().Add((Control)(object)label5);
			((Control)groupBox3).get_Controls().Add((Control)(object)label6);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnDECdeg);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnRAhrs);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)label3);
			((Control)groupBox3).get_Controls().Add((Control)(object)label2);
			((Control)groupBox3).get_Controls().Add((Control)(object)label1);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(7, 172));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(825, 250));
			((Control)groupBox3).set_TabIndex(1);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("Set field locations");
			((Control)lstTest).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstTest).set_FormattingEnabled(true);
			lstTest.set_HorizontalExtent(1800);
			lstTest.set_HorizontalScrollbar(true);
			lstTest.set_ItemHeight(14);
			((Control)lstTest).set_Location(new Point(122, 144));
			((Control)lstTest).set_Name("lstTest");
			lstTest.set_ScrollAlwaysVisible(true);
			((Control)lstTest).set_Size(new Size(692, 102));
			((Control)lstTest).set_TabIndex(40);
			((Control)cmdTestRead).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTestRead).set_Location(new Point(6, 183));
			((Control)cmdTestRead).set_Name("cmdTestRead");
			((Control)cmdTestRead).set_Size(new Size(103, 25));
			((Control)cmdTestRead).set_TabIndex(39);
			((Control)cmdTestRead).set_Text("Test read");
			((ButtonBase)cmdTestRead).set_UseVisualStyleBackColor(true);
			((Control)cmdTestRead).add_Click((EventHandler)cmdTestRead_Click);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_Location(new Point(551, 91));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(36, 13));
			((Control)label23).set_TabIndex(36);
			((Control)label23).set_Text("# cols");
			((Control)updnIdentifierCols).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtIDCols", true, (DataSourceUpdateMode)1));
			((Control)updnIdentifierCols).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnIdentifierCols).set_Location(new Point(553, 106));
			updnIdentifierCols.set_Maximum(new decimal(new int[4] { 9, 0, 0, 0 }));
			updnIdentifierCols.set_Minimum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnIdentifierCols).set_Name("updnIdentifierCols");
			((Control)updnIdentifierCols).set_Size(new Size(30, 20));
			((Control)updnIdentifierCols).set_TabIndex(37);
			updnIdentifierCols.set_Value(Settings.Default.CatCvrtIDCols);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(496, 91));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(49, 13));
			((Control)label24).set_TabIndex(34);
			((Control)label24).set_Text("Start col.");
			((Control)updnIdentifier).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtID", true, (DataSourceUpdateMode)1));
			((Control)updnIdentifier).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnIdentifier).set_Location(new Point(499, 106));
			updnIdentifier.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnIdentifier.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnIdentifier).set_Name("updnIdentifier");
			((Control)updnIdentifier).set_Size(new Size(43, 20));
			((Control)updnIdentifier).set_TabIndex(35);
			updnIdentifier.set_Value(Settings.Default.CatCvrtID);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(450, 101));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(47, 26));
			((Control)label26).set_TabIndex(33);
			((Control)label26).set_Text("Numeric\r\nIdentifier");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(352, 91));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(36, 13));
			((Control)label19).set_TabIndex(31);
			((Control)label19).set_Text("# cols");
			((Control)updnMRCols).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtMrCols", true, (DataSourceUpdateMode)1));
			((Control)updnMRCols).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMRCols).set_Location(new Point(354, 106));
			updnMRCols.set_Maximum(new decimal(new int[4] { 6, 0, 0, 0 }));
			((Control)updnMRCols).set_Name("updnMRCols");
			((Control)updnMRCols).set_Size(new Size(30, 20));
			((Control)updnMRCols).set_TabIndex(32);
			updnMRCols.set_Value(Settings.Default.CatCvrtMrCols);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(297, 91));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(49, 13));
			((Control)label20).set_TabIndex(29);
			((Control)label20).set_Text("Start col.");
			((Control)updnMR).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtMr", true, (DataSourceUpdateMode)1));
			((Control)updnMR).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMR).set_Location(new Point(300, 106));
			updnMR.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnMR.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMR).set_Name("updnMR");
			((Control)updnMR).set_Size(new Size(43, 20));
			((Control)updnMR).set_TabIndex(30);
			updnMR.set_Value(Settings.Default.CatCvrtMr);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(266, 110));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(27, 13));
			((Control)label22).set_TabIndex(28);
			((Control)label22).set_Text("Red");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(189, 91));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(36, 13));
			((Control)label8).set_TabIndex(26);
			((Control)label8).set_Text("# cols");
			((Control)updnMVCols).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtMvCols", true, (DataSourceUpdateMode)1));
			((Control)updnMVCols).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMVCols).set_Location(new Point(191, 106));
			updnMVCols.set_Maximum(new decimal(new int[4] { 6, 0, 0, 0 }));
			((Control)updnMVCols).set_Name("updnMVCols");
			((Control)updnMVCols).set_Size(new Size(30, 20));
			((Control)updnMVCols).set_TabIndex(27);
			updnMVCols.set_Value(Settings.Default.CatCvrtMvCols);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(134, 91));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(49, 13));
			((Control)label9).set_TabIndex(24);
			((Control)label9).set_Text("Start col.");
			((Control)updnMV).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtMv", true, (DataSourceUpdateMode)1));
			((Control)updnMV).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMV).set_Location(new Point(137, 106));
			updnMV.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnMV.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMV).set_Name("updnMV");
			((Control)updnMV).set_Size(new Size(43, 20));
			((Control)updnMV).set_TabIndex(25);
			updnMV.set_Value(Settings.Default.CatCvrtMv);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(97, 110));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(35, 13));
			((Control)label10).set_TabIndex(23);
			((Control)label10).set_Text("Visual");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(10, 110));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(72, 13));
			((Control)label7).set_TabIndex(22);
			((Control)label7).set_Text("Magnitudes");
			((Control)chkDECSecHasDecimal).set_AutoSize(true);
			chkDECSecHasDecimal.set_Checked(Settings.Default.CatCvrtDECsecsDecimal);
			chkDECSecHasDecimal.set_CheckState((CheckState)1);
			((Control)chkDECSecHasDecimal).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CatCvrtDECsecsDecimal", true, (DataSourceUpdateMode)1));
			((Control)chkDECSecHasDecimal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDECSecHasDecimal).set_Location(new Point(519, 58));
			((Control)chkDECSecHasDecimal).set_Name("chkDECSecHasDecimal");
			((Control)chkDECSecHasDecimal).set_Size(new Size(131, 17));
			((Control)chkDECSecHasDecimal).set_TabIndex(21);
			((Control)chkDECSecHasDecimal).set_Text("Includes decimal point");
			((ButtonBase)chkDECSecHasDecimal).set_UseVisualStyleBackColor(true);
			((Control)chkRAsecHasDecimal).set_AutoSize(true);
			chkRAsecHasDecimal.set_Checked(Settings.Default.CatCvrtRAsecsDecimal);
			chkRAsecHasDecimal.set_CheckState((CheckState)1);
			((Control)chkRAsecHasDecimal).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CatCvrtRAsecsDecimal", true, (DataSourceUpdateMode)1));
			((Control)chkRAsecHasDecimal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkRAsecHasDecimal).set_Location(new Point(519, 35));
			((Control)chkRAsecHasDecimal).set_Name("chkRAsecHasDecimal");
			((Control)chkRAsecHasDecimal).set_Size(new Size(131, 17));
			((Control)chkRAsecHasDecimal).set_TabIndex(12);
			((Control)chkRAsecHasDecimal).set_Text("Includes decimal point");
			((ButtonBase)chkRAsecHasDecimal).set_UseVisualStyleBackColor(true);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(469, 16));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(36, 13));
			((Control)label21).set_TabIndex(10);
			((Control)label21).set_Text("# cols");
			((Control)updnDECSecsLength).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtDECsecsCols", true, (DataSourceUpdateMode)1));
			((Control)updnDECSecsLength).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDECSecsLength).set_Location(new Point(471, 56));
			updnDECSecsLength.set_Maximum(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnDECSecsLength.set_Minimum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnDECSecsLength).set_Name("updnDECSecsLength");
			((Control)updnDECSecsLength).set_Size(new Size(30, 20));
			((Control)updnDECSecsLength).set_TabIndex(20);
			updnDECSecsLength.set_Value(Settings.Default.CatCvrtDECsecsCols);
			((Control)updnRASecsLength).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtRAsecsCols", true, (DataSourceUpdateMode)1));
			((Control)updnRASecsLength).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnRASecsLength).set_Location(new Point(471, 33));
			updnRASecsLength.set_Maximum(new decimal(new int[4] { 8, 0, 0, 0 }));
			updnRASecsLength.set_Minimum(new decimal(new int[4] { 2, 0, 0, 0 }));
			((Control)updnRASecsLength).set_Name("updnRASecsLength");
			((Control)updnRASecsLength).set_Size(new Size(30, 20));
			((Control)updnRASecsLength).set_TabIndex(11);
			updnRASecsLength.set_Value(Settings.Default.CatCvrtRAsecsCols);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(415, 16));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(49, 13));
			((Control)label18).set_TabIndex(8);
			((Control)label18).set_Text("Start col.");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(294, 16));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(49, 13));
			((Control)label17).set_TabIndex(5);
			((Control)label17).set_Text("Start col.");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(130, 16));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(49, 13));
			((Control)label15).set_TabIndex(3);
			((Control)label15).set_Text("Start col.");
			((Control)updnDECSec).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtDECsecs", true, (DataSourceUpdateMode)1));
			((Control)updnDECSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDECSec).set_Location(new Point(418, 56));
			updnDECSec.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnDECSec.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDECSec).set_Name("updnDECSec");
			((Control)updnDECSec).set_Size(new Size(43, 20));
			((Control)updnDECSec).set_TabIndex(19);
			updnDECSec.set_Value(Settings.Default.CatCvrtDECsecs);
			((Control)updnRASecs).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtRAsecs", true, (DataSourceUpdateMode)1));
			((Control)updnRASecs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnRASecs).set_Location(new Point(418, 33));
			updnRASecs.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnRASecs.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnRASecs).set_Name("updnRASecs");
			((Control)updnRASecs).set_Size(new Size(43, 20));
			((Control)updnRASecs).set_TabIndex(9);
			updnRASecs.set_Value(Settings.Default.CatCvrtRAsecs);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(384, 37));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(26, 13));
			((Control)label11).set_TabIndex(7);
			((Control)label11).set_Text("Sec");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(384, 60));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(26, 13));
			((Control)label12).set_TabIndex(18);
			((Control)label12).set_Text("Sec");
			((Control)updnDECMin).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtDECmin", true, (DataSourceUpdateMode)1));
			((Control)updnDECMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDECMin).set_Location(new Point(297, 56));
			updnDECMin.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnDECMin.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDECMin).set_Name("updnDECMin");
			((Control)updnDECMin).set_Size(new Size(43, 20));
			((Control)updnDECMin).set_TabIndex(17);
			updnDECMin.set_Value(Settings.Default.CatCvrtDECmin);
			((Control)updnRAMins).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtRAmins", true, (DataSourceUpdateMode)1));
			((Control)updnRAMins).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnRAMins).set_Location(new Point(297, 33));
			updnRAMins.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnRAMins.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnRAMins).set_Name("updnRAMins");
			((Control)updnRAMins).set_Size(new Size(43, 20));
			((Control)updnRAMins).set_TabIndex(6);
			updnRAMins.set_Value(Settings.Default.CatCvrtRAmins);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(214, 37));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(74, 13));
			((Control)label5).set_TabIndex(4);
			((Control)label5).set_Text("Min:  (2 chars)");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(214, 60));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(74, 13));
			((Control)label6).set_TabIndex(16);
			((Control)label6).set_Text("Min:  (2 chars)");
			((Control)updnDECdeg).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtDECdeg", true, (DataSourceUpdateMode)1));
			((Control)updnDECdeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDECdeg).set_Location(new Point(133, 56));
			updnDECdeg.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnDECdeg.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDECdeg).set_Name("updnDECdeg");
			((Control)updnDECdeg).set_Size(new Size(43, 20));
			((Control)updnDECdeg).set_TabIndex(15);
			updnDECdeg.set_Value(Settings.Default.CatCvrtDECdeg);
			((Control)updnRAhrs).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "CatCvrtRAhrs", true, (DataSourceUpdateMode)1));
			((Control)updnRAhrs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnRAhrs).set_Location(new Point(133, 33));
			updnRAhrs.set_Maximum(new decimal(new int[4] { 900, 0, 0, 0 }));
			updnRAhrs.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnRAhrs).set_Name("updnRAhrs");
			((Control)updnRAhrs).set_Size(new Size(43, 20));
			((Control)updnRAhrs).set_TabIndex(2);
			updnRAhrs.set_Value(Settings.Default.CatCvrtRAhrs);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(10, 60));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(30, 13));
			((Control)label4).set_TabIndex(13);
			((Control)label4).set_Text("Dec");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(10, 37));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(32, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("R.A.");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(50, 37));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(73, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Hrs:  (2 chars)");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(50, 60));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(77, 13));
			((Control)label1).set_TabIndex(14);
			((Control)label1).set_Text("Deg:  (3 chars)");
			((Control)groupBox4).get_Controls().Add((Control)(object)cmdCreateUser);
			((Control)groupBox4).get_Controls().Add((Control)(object)optB);
			((Control)groupBox4).get_Controls().Add((Control)(object)optD);
			((Control)groupBox4).get_Controls().Add((Control)(object)optA);
			((Control)groupBox4).get_Controls().Add((Control)(object)optE);
			((Control)groupBox4).get_Controls().Add((Control)(object)optC);
			((Control)groupBox4).get_Controls().Add((Control)(object)optF);
			((Control)groupBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox4).set_Location(new Point(7, 430));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(825, 103));
			((Control)groupBox4).set_TabIndex(2);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("Select output catalogue, and create");
			((Control)cmdCreateUser).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdCreateUser).set_Location(new Point(336, 39));
			((Control)cmdCreateUser).set_Name("cmdCreateUser");
			((Control)cmdCreateUser).set_Size(new Size(127, 27));
			((Control)cmdCreateUser).set_TabIndex(6);
			((Control)cmdCreateUser).set_Text("Create User catalogue");
			((ButtonBase)cmdCreateUser).set_UseVisualStyleBackColor(true);
			((Control)cmdCreateUser).add_Click((EventHandler)cmdCreateUser_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(843, 24));
			((Control)menuStrip1).set_TabIndex(3);
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
			((Control)chkPrecess).set_AutoSize(true);
			((Control)chkPrecess).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPrecess).set_Location(new Point(687, 40));
			((Control)chkPrecess).set_Name("chkPrecess");
			((Control)chkPrecess).set_Size(new Size(89, 30));
			((Control)chkPrecess).set_TabIndex(38);
			((Control)chkPrecess).set_Text("Precess from\r\n1950 to 2000");
			((ButtonBase)chkPrecess).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(843, 549));
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsCreateUserCatalogue", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationStarsCreateUserCatalogue);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("CreateUserCatalogue");
			((Control)this).set_Text("Create User catalogue for Asteroid search, from user's astrometry");
			((Form)this).add_Load((EventHandler)CreateUserCatalogue_Load);
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((ISupportInitialize)updnIdentifierCols).EndInit();
			((ISupportInitialize)updnIdentifier).EndInit();
			((ISupportInitialize)updnMRCols).EndInit();
			((ISupportInitialize)updnMR).EndInit();
			((ISupportInitialize)updnMVCols).EndInit();
			((ISupportInitialize)updnMV).EndInit();
			((ISupportInitialize)updnDECSecsLength).EndInit();
			((ISupportInitialize)updnRASecsLength).EndInit();
			((ISupportInitialize)updnDECSec).EndInit();
			((ISupportInitialize)updnRASecs).EndInit();
			((ISupportInitialize)updnDECMin).EndInit();
			((ISupportInitialize)updnRAMins).EndInit();
			((ISupportInitialize)updnDECdeg).EndInit();
			((ISupportInitialize)updnRAhrs).EndInit();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
