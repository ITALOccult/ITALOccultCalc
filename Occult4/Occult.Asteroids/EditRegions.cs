using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class EditRegions : Form
	{
		private string RegionFile = Utilities.AppPath + "\\Resource Files\\AsteroidRegions.txt";

		private string[] Regions = new string[100];

		private int CurrentlySelected = -1;

		private bool UnsavedEdits;

		private IContainer components;

		private ListBox lstRegions;

		private NumericUpDown updnLongNW;

		private NumericUpDown updnLongSW;

		private NumericUpDown updnLongSE;

		private NumericUpDown updnLongNE;

		private NumericUpDown updnLatNW;

		private NumericUpDown updnLatSW;

		private NumericUpDown updnLatSE;

		private NumericUpDown updnLatNE;

		private Label label23;

		private Label label24;

		private Label label21;

		private Label label22;

		private Label label20;

		private Label label19;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label26;

		private Label label25;

		private TextBox txtName;

		private Button cmdAdd;

		private Button cmdReplace;

		private Button cmdDelete;

		private Button cmdSave;

		private Button cmdGoogleEarth;

		private Label label5;

		private Panel panel1;

		private Label label7;

		private Label label6;

		private Button cmdHelp;

		public EditRegions()
		{
			InitializeComponent();
		}

		private void EditRegions_Load(object sender, EventArgs e)
		{
			for (int i = 0; i < 100; i++)
			{
				Regions[i] = "";
			}
			if (!File.Exists(RegionFile))
			{
				return;
			}
			if (new FileInfo(RegionFile).Length > 20)
			{
				using (StreamReader streamReader = new StreamReader(RegionFile))
				{
					int num = 0;
					int num2 = 0;
					do
					{
						Regions[num] = streamReader.ReadLine();
						num2 = Regions[num].IndexOf(",");
						lstRegions.get_Items().Add((object)Regions[num].Substring(0, num2));
						num++;
					}
					while (!streamReader.EndOfStream);
				}
				CurrentlySelected = 0;
				((ListControl)lstRegions).set_SelectedIndex(0);
			}
			UnsavedEdits = false;
		}

		private void lstRegions_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstRegions).get_SelectedIndex() > -1)
			{
				CurrentlySelected = ((ListControl)lstRegions).get_SelectedIndex();
				string[] array = Regions[CurrentlySelected].ToString().Split(new char[1] { ',' });
				int length = array.GetLength(0);
				if (length >= 1)
				{
					((Control)txtName).set_Text(array[0]);
				}
				if (length >= 2)
				{
					updnLongNW.set_Value(decimal.Parse(array[1]));
				}
				if (length >= 3)
				{
					updnLatNW.set_Value(decimal.Parse(array[2]));
				}
				if (length >= 4)
				{
					updnLongNE.set_Value(decimal.Parse(array[3]));
				}
				if (length >= 5)
				{
					updnLatNE.set_Value(decimal.Parse(array[4]));
				}
				if (length >= 6)
				{
					updnLongSW.set_Value(decimal.Parse(array[5]));
				}
				if (length >= 7)
				{
					updnLatSW.set_Value(decimal.Parse(array[6]));
				}
				if (length >= 8)
				{
					updnLongSE.set_Value(decimal.Parse(array[7]));
				}
				if (length >= 9)
				{
					updnLatSE.set_Value(decimal.Parse(array[8]));
				}
			}
		}

		private string RegionLine()
		{
			return string.Concat(string.Concat(string.Concat(((Control)txtName).get_Text() + string.Format(",{0,1:f0},{1,1:f0}", updnLongNW.get_Value(), updnLatNW.get_Value()), string.Format(",{0,1:f0},{1,1:f0}", updnLongNE.get_Value(), updnLatNE.get_Value())), string.Format(",{0,1:f0},{1,1:f0}", updnLongSW.get_Value(), updnLatSW.get_Value())), string.Format(",{0,1:f0},{1,1:f0}", updnLongSE.get_Value(), updnLatSE.get_Value()));
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			Save();
		}

		private void Save()
		{
			using (StreamWriter streamWriter = new StreamWriter(RegionFile))
			{
				for (int i = 0; i < lstRegions.get_Items().get_Count(); i++)
				{
					streamWriter.WriteLine(Regions[i]);
				}
			}
			UnsavedEdits = false;
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			int num = lstRegions.get_Items().Add((object)((Control)txtName).get_Text());
			Regions[num] = RegionLine();
			SortRegions(num, FollowingDeletion: false);
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			//IL_0041: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to replace the entry for " + lstRegions.get_Items().get_Item(((ListControl)lstRegions).get_SelectedIndex()).ToString() + " ?", "Confirm replacement", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				Regions[CurrentlySelected] = RegionLine();
				SortRegions(CurrentlySelected, FollowingDeletion: false);
			}
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0041: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to delete the entry for " + lstRegions.get_Items().get_Item(((ListControl)lstRegions).get_SelectedIndex()).ToString() + " ?", "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				for (int i = CurrentlySelected; i < 99; i++)
				{
					Regions[i] = Regions[i + 1];
				}
				lstRegions.get_Items().RemoveAt(CurrentlySelected);
				SortRegions(CurrentlySelected, FollowingDeletion: true);
			}
		}

		private void EditRegions_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Invalid comparison between Unknown and I4
			if (UnsavedEdits && (int)MessageBox.Show("You have unsaved edits. Do you want to save them now?", "Unsaved edits", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
			{
				Save();
			}
		}

		private void cmdGoogleEarth_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			GoogleEarth.Create_New_GoogleEarthKMZ_File(Utilities.AppPath + "\\Predictions\\AsteroidRegions.KMZ", "Region for " + ((Control)txtName).get_Text(), AutoOpenFile: true, out var CreatedFile);
			GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(0);
			GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ((double)updnLongNW.get_Value(), (double)updnLatNW.get_Value(), 0.0);
			GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ((double)updnLongNE.get_Value(), (double)updnLatNE.get_Value(), 0.0);
			GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ((double)updnLongSE.get_Value(), (double)updnLatSE.get_Value(), 0.0);
			GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ((double)updnLongSW.get_Value(), (double)updnLatSW.get_Value(), 0.0);
			GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ((double)updnLongNW.get_Value(), (double)updnLatNW.get_Value(), 0.0);
			GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
			GoogleEarth.DisplayGoogleMap(CreatedFile);
		}

		private void SortRegions(int SelectedLine, bool FollowingDeletion)
		{
			string text = "";
			int num = -1;
			if (FollowingDeletion)
			{
				SelectedLine--;
				if ((SelectedLine < 0) & (lstRegions.get_Items().get_Count() > 0))
				{
					SelectedLine = 0;
				}
			}
			if (SelectedLine >= 0)
			{
				text = Regions[SelectedLine];
				Array.Sort(Regions, 0, lstRegions.get_Items().get_Count(), (IComparer<string>?)StringComparer.CurrentCulture);
				lstRegions.get_Items().Clear();
				for (int i = 0; i < 100 && Regions[i].Length > 0; i++)
				{
					int length = Regions[i].IndexOf(",");
					lstRegions.get_Items().Add((object)Regions[i].Substring(0, length));
					if (Regions[i] == text)
					{
						num = i;
					}
				}
			}
			if (num >= 0)
			{
				((ListControl)lstRegions).set_SelectedIndex(num);
			}
			UnsavedEdits = true;
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid - Edit Regions");
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
			//IL_1485: Unknown result type (might be due to invalid IL or missing references)
			//IL_148f: Expected O, but got Unknown
			lstRegions = new ListBox();
			updnLongNW = new NumericUpDown();
			updnLongSW = new NumericUpDown();
			updnLongSE = new NumericUpDown();
			updnLongNE = new NumericUpDown();
			updnLatNW = new NumericUpDown();
			updnLatSW = new NumericUpDown();
			updnLatSE = new NumericUpDown();
			updnLatNE = new NumericUpDown();
			label23 = new Label();
			label24 = new Label();
			label21 = new Label();
			label22 = new Label();
			label20 = new Label();
			label19 = new Label();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label26 = new Label();
			label25 = new Label();
			txtName = new TextBox();
			cmdAdd = new Button();
			cmdReplace = new Button();
			cmdDelete = new Button();
			cmdSave = new Button();
			cmdGoogleEarth = new Button();
			label5 = new Label();
			panel1 = new Panel();
			label7 = new Label();
			label6 = new Label();
			cmdHelp = new Button();
			((ISupportInitialize)updnLongNW).BeginInit();
			((ISupportInitialize)updnLongSW).BeginInit();
			((ISupportInitialize)updnLongSE).BeginInit();
			((ISupportInitialize)updnLongNE).BeginInit();
			((ISupportInitialize)updnLatNW).BeginInit();
			((ISupportInitialize)updnLatSW).BeginInit();
			((ISupportInitialize)updnLatSE).BeginInit();
			((ISupportInitialize)updnLatNE).BeginInit();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((ListControl)lstRegions).set_FormattingEnabled(true);
			((Control)lstRegions).set_Location(new Point(14, 23));
			((Control)lstRegions).set_Name("lstRegions");
			((Control)lstRegions).set_Size(new Size(178, 264));
			((Control)lstRegions).set_TabIndex(0);
			lstRegions.add_SelectedIndexChanged((EventHandler)lstRegions_SelectedIndexChanged);
			((Control)updnLongNW).set_Location(new Point(30, 64));
			updnLongNW.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongNW.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongNW).set_Name("updnLongNW");
			((Control)updnLongNW).set_Size(new Size(42, 20));
			((Control)updnLongNW).set_TabIndex(1);
			((Control)updnLongSW).set_Location(new Point(30, 118));
			updnLongSW.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongSW.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongSW).set_Name("updnLongSW");
			((Control)updnLongSW).set_Size(new Size(42, 20));
			((Control)updnLongSW).set_TabIndex(2);
			((Control)updnLongSE).set_Location(new Point(167, 118));
			updnLongSE.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongSE.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongSE).set_Name("updnLongSE");
			((Control)updnLongSE).set_Size(new Size(42, 20));
			((Control)updnLongSE).set_TabIndex(3);
			((Control)updnLongNE).set_Location(new Point(167, 64));
			updnLongNE.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnLongNE.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnLongNE).set_Name("updnLongNE");
			((Control)updnLongNE).set_Size(new Size(42, 20));
			((Control)updnLongNE).set_TabIndex(4);
			((Control)updnLatNW).set_Location(new Point(78, 64));
			updnLatNW.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatNW.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatNW).set_Name("updnLatNW");
			((Control)updnLatNW).set_Size(new Size(35, 20));
			((Control)updnLatNW).set_TabIndex(5);
			((Control)updnLatSW).set_Location(new Point(78, 118));
			updnLatSW.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatSW.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatSW).set_Name("updnLatSW");
			((Control)updnLatSW).set_Size(new Size(35, 20));
			((Control)updnLatSW).set_TabIndex(6);
			((Control)updnLatSE).set_Location(new Point(215, 118));
			updnLatSE.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatSE.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatSE).set_Name("updnLatSE");
			((Control)updnLatSE).set_Size(new Size(35, 20));
			((Control)updnLatSE).set_TabIndex(7);
			((Control)updnLatNE).set_Location(new Point(215, 64));
			updnLatNE.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnLatNE.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnLatNE).set_Name("updnLatNE");
			((Control)updnLatNE).set_Size(new Size(35, 20));
			((Control)updnLatNE).set_TabIndex(8);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_ForeColor(Color.MediumBlue);
			((Control)label23).set_Location(new Point(59, 34));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(23, 13));
			((Control)label23).set_TabIndex(18);
			((Control)label23).set_Text("NW");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_ForeColor(Color.MediumBlue);
			((Control)label24).set_Location(new Point(200, 38));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(21, 13));
			((Control)label24).set_TabIndex(23);
			((Control)label24).set_Text("NE");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(171, 48));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(34, 13));
			((Control)label21).set_TabIndex(21);
			((Control)label21).set_Text("Long.");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(215, 48));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(25, 13));
			((Control)label22).set_TabIndex(22);
			((Control)label22).set_Text("Lat.");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(32, 48));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(34, 13));
			((Control)label20).set_TabIndex(19);
			((Control)label20).set_Text("Long.");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(76, 48));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(25, 13));
			((Control)label19).set_TabIndex(20);
			((Control)label19).set_Text("Lat.");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(171, 102));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(34, 13));
			((Control)label1).set_TabIndex(26);
			((Control)label1).set_Text("Long.");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(215, 102));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(25, 13));
			((Control)label2).set_TabIndex(27);
			((Control)label2).set_Text("Lat.");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(32, 102));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(34, 13));
			((Control)label3).set_TabIndex(24);
			((Control)label3).set_Text("Long.");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(76, 102));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(25, 13));
			((Control)label4).set_TabIndex(25);
			((Control)label4).set_Text("Lat.");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_ForeColor(Color.MediumBlue);
			((Control)label26).set_Location(new Point(59, 91));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(23, 13));
			((Control)label26).set_TabIndex(29);
			((Control)label26).set_Text("SW");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_ForeColor(Color.MediumBlue);
			((Control)label25).set_Location(new Point(200, 89));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(21, 13));
			((Control)label25).set_TabIndex(28);
			((Control)label25).set_Text("SE");
			((Control)txtName).set_Location(new Point(122, 7));
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(123, 20));
			((Control)txtName).set_TabIndex(30);
			((Control)txtName).set_Text("Region label");
			((Control)cmdAdd).set_Location(new Point(213, 208));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(88, 36));
			((Control)cmdAdd).set_TabIndex(31);
			((Control)cmdAdd).set_Text("Add as new Region");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)cmdReplace).set_Location(new Point(319, 208));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(88, 36));
			((Control)cmdReplace).set_TabIndex(32);
			((Control)cmdReplace).set_Text("Replace selected region");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(true);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)cmdDelete).set_Location(new Point(319, 250));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(88, 36));
			((Control)cmdDelete).set_TabIndex(33);
			((Control)cmdDelete).set_Text("Delete selected region");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)cmdSave).set_Location(new Point(425, 229));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(88, 36));
			((Control)cmdSave).set_TabIndex(34);
			((Control)cmdSave).set_Text("Save regions");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)cmdGoogleEarth).set_Location(new Point(213, 250));
			((Control)cmdGoogleEarth).set_Name("cmdGoogleEarth");
			((Control)cmdGoogleEarth).set_Size(new Size(88, 36));
			((Control)cmdGoogleEarth).set_TabIndex(35);
			((Control)cmdGoogleEarth).set_Text("Display in GoogleEarth");
			((ButtonBase)cmdGoogleEarth).set_UseVisualStyleBackColor(true);
			((Control)cmdGoogleEarth).add_Click((EventHandler)cmdGoogleEarth_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(67, 6));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(72, 13));
			((Control)label5).set_TabIndex(36);
			((Control)label5).set_Text("List of regions");
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)txtName);
			((Control)panel1).get_Controls().Add((Control)(object)label26);
			((Control)panel1).get_Controls().Add((Control)(object)label25);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label24);
			((Control)panel1).get_Controls().Add((Control)(object)label21);
			((Control)panel1).get_Controls().Add((Control)(object)label22);
			((Control)panel1).get_Controls().Add((Control)(object)label20);
			((Control)panel1).get_Controls().Add((Control)(object)label19);
			((Control)panel1).get_Controls().Add((Control)(object)label23);
			((Control)panel1).get_Controls().Add((Control)(object)updnLatNE);
			((Control)panel1).get_Controls().Add((Control)(object)updnLatSE);
			((Control)panel1).get_Controls().Add((Control)(object)updnLatSW);
			((Control)panel1).get_Controls().Add((Control)(object)updnLatNW);
			((Control)panel1).get_Controls().Add((Control)(object)updnLongNE);
			((Control)panel1).get_Controls().Add((Control)(object)updnLongSE);
			((Control)panel1).get_Controls().Add((Control)(object)updnLongSW);
			((Control)panel1).get_Controls().Add((Control)(object)updnLongNW);
			((Control)panel1).set_Location(new Point(218, 51));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(290, 149));
			((Control)panel1).set_TabIndex(37);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(43, 10));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(78, 13));
			((Control)label7).set_TabIndex(31);
			((Control)label7).set_Text("Region label = ");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(217, 10));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(263, 39));
			((Control)label6).set_TabIndex(38);
			((Control)label6).set_Text("Set the Longitude and Latitude coordinates for the\r\ncorners of the quadrilateral to use for filtering asteroidal\r\noccultation predictions.");
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.TopLeft);
			((Control)cmdHelp).set_Location(new Point(482, 2));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(52, 25));
			((Control)cmdHelp).set_TabIndex(39);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(535, 295));
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)cmdGoogleEarth);
			((Control)this).get_Controls().Add((Control)(object)cmdSave);
			((Control)this).get_Controls().Add((Control)(object)cmdDelete);
			((Control)this).get_Controls().Add((Control)(object)cmdReplace);
			((Control)this).get_Controls().Add((Control)(object)cmdAdd);
			((Control)this).get_Controls().Add((Control)(object)lstRegions);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("EditRegions");
			((Control)this).set_Text("Edit regions for List and Display filter");
			((Form)this).add_FormClosing(new FormClosingEventHandler(EditRegions_FormClosing));
			((Form)this).add_Load((EventHandler)EditRegions_Load);
			((ISupportInitialize)updnLongNW).EndInit();
			((ISupportInitialize)updnLongSW).EndInit();
			((ISupportInitialize)updnLongSE).EndInit();
			((ISupportInitialize)updnLongNE).EndInit();
			((ISupportInitialize)updnLatNW).EndInit();
			((ISupportInitialize)updnLatSW).EndInit();
			((ISupportInitialize)updnLatSE).EndInit();
			((ISupportInitialize)updnLatNE).EndInit();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
