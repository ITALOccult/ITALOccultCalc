using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class AsteroidID_SearchString : Form
	{
		public int FirstRecord;

		public int LastRecord;

		public int FirstAsteroidNumber;

		public int LastAsteroidNumber;

		public string FirstAsteroidName = "";

		public string LastAsteroidName = "";

		public string SearchLabel = "None meeting criteria";

		public int NumberOfAsteroids = -1;

		private string PreSetAsteroid_ID;

		private bool UserElements;

		private IContainer components;

		private Button cmdOK;

		private RadioButton optSelectAll;

		private RadioButton optSelectRange;

		private RadioButton optSelectAllNumbered;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label7;

		private Label label8;

		private Label label6;

		private Label label9;

		private Button cmdCancel;

		private Panel panel1;

		private Button cmdSelectFromList;

		private Label label1;

		private Label label11;

		private Label label10;

		private Label label2;

		private Panel panel2;

		private Label label13;

		private Label label12;

		private Label label14;

		private Label label16;

		internal TextBox txtSearchString;

		public AsteroidID_SearchString(bool UseUserElements, string PreSetAsteroidID)
		{
			UserElements = UseUserElements;
			InitializeComponent();
			if (UserElements)
			{
				((Control)this).set_Text("Set search asteroids from User file of asteroid lements");
			}
			else
			{
				((Control)this).set_Text("Set search asteroids from main file of asteroid elements");
			}
			PreSetAsteroid_ID = PreSetAsteroidID;
			((Control)txtSearchString).Focus();
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			if (SetValues())
			{
				((Form)this).set_DialogResult((DialogResult)1);
				((Form)this).Close();
			}
			else
			{
				MessageBox.Show("Please re-enter the data", "Data entry error", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void AsteroidID_SearchString_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			if (PreSetAsteroid_ID.Length > 0)
			{
				((Control)txtSearchString).set_Text(PreSetAsteroid_ID);
				ProcessTextOnPressEnter();
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
		}

		private void txtSearchString_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.get_KeyChar() == '\r')
			{
				ProcessTextOnPressEnter();
			}
		}

		private void ProcessTextOnPressEnter()
		{
			//IL_002f: Unknown result type (might be due to invalid IL or missing references)
			optSelectRange.set_Checked(true);
			if (SetValues())
			{
				((Form)this).set_DialogResult((DialogResult)1);
				((Form)this).Close();
			}
			else
			{
				MessageBox.Show("Please re-enter the data", "Data entry error", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void txtSearchString_KeyDown(object sender, KeyEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0008: Invalid comparison between Unknown and I4
			if ((int)e.get_KeyCode() == 27)
			{
				((Form)this).set_DialogResult((DialogResult)2);
				((Form)this).Close();
			}
		}

		private void cmdSelectFromList_Click(object sender, EventArgs e)
		{
			SelectFromList("");
		}

		private bool SelectFromList(string SelectionString)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0014: Invalid comparison between Unknown and I4
			AsteroidSelectForSearch asteroidSelectForSearch = new AsteroidSelectForSearch(UserElements, SelectionString);
			if ((int)((Form)asteroidSelectForSearch).ShowDialog() == 1)
			{
				FirstRecord = asteroidSelectForSearch.FirstRecord;
				LastRecord = asteroidSelectForSearch.LastRecord;
				FirstAsteroidName = asteroidSelectForSearch.FirstAsteroidName;
				FirstAsteroidNumber = asteroidSelectForSearch.FirstAsteroidNumber;
				LastAsteroidName = asteroidSelectForSearch.LastAsteroidName;
				LastAsteroidNumber = asteroidSelectForSearch.LastAsteroidNumber;
				string text = "";
				if (FirstAsteroidNumber > 0)
				{
					text = "(" + FirstAsteroidNumber + ") ";
				}
				string text2 = "";
				if (LastAsteroidNumber > 0)
				{
					text2 = "(" + LastAsteroidNumber + ") ";
				}
				SearchLabel = text + FirstAsteroidName + " <=> " + text2 + LastAsteroidName;
				((Form)this).set_DialogResult((DialogResult)1);
				((Form)this).Close();
				return true;
			}
			return false;
		}

		private bool SetValues()
		{
			if (!UserElements)
			{
				if (Elements.MainAsteroids.AstElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
			}
			else if (Elements.UserAsteroids.AstElements.Count < 5)
			{
				Elements.UserAsteroids.Fill_AllAsteroids();
			}
			int result = 0;
			string text = "";
			FirstAsteroidNumber = 0;
			LastAsteroidNumber = 0;
			FirstAsteroidName = "";
			LastAsteroidName = "";
			if (optSelectAll.get_Checked())
			{
				FirstRecord = 0;
				if (!UserElements)
				{
					LastRecord = Elements.MainAsteroids.AstElements.Count - 1;
				}
				else
				{
					LastRecord = Elements.UserAsteroids.AstElements.Count - 1;
				}
			}
			else if (optSelectAllNumbered.get_Checked())
			{
				FirstRecord = 0;
				if (!UserElements)
				{
					LastRecord = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(999999, GetNextHigher: false, GetNextLower: true);
				}
				else
				{
					LastRecord = Elements.UserAsteroids.GetAsteroidRecord_fromNumber(999999, GetNextHigher: false, GetNextLower: true);
				}
			}
			else if (optSelectRange.get_Checked())
			{
				if (((Control)txtSearchString).get_Text().Trim() == "")
				{
					return false;
				}
				if (((Control)txtSearchString).get_Text().Trim().Contains("*"))
				{
					return SelectFromList(((Control)txtSearchString).get_Text().Trim());
				}
				string[] array = ((Control)txtSearchString).get_Text().Trim().Split(new char[1] { '-' });
				if (array.Length == 1)
				{
					if (array[0].Trim().Length == 0)
					{
						return false;
					}
					if (!int.TryParse(array[0], out result))
					{
						text = array[0];
						if (!UserElements)
						{
							FirstRecord = (LastRecord = Elements.MainAsteroids.GetAsteroidRecord_fromName(array[0].Trim()));
						}
						else
						{
							FirstRecord = (LastRecord = Elements.UserAsteroids.GetAsteroidRecord_fromName(array[0].Trim()));
						}
					}
					else if (!UserElements)
					{
						FirstRecord = (LastRecord = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result));
					}
					else
					{
						FirstRecord = (LastRecord = Elements.UserAsteroids.GetAsteroidRecord_fromNumber(result));
					}
				}
				else
				{
					if ((array[0].Trim().Length == 0) & (array[1].Trim().Length == 0))
					{
						return false;
					}
					int.TryParse(array[0], out FirstAsteroidNumber);
					if (FirstAsteroidNumber == 0)
					{
						FirstAsteroidNumber = 1;
					}
					if (!UserElements)
					{
						FirstRecord = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(FirstAsteroidNumber, GetNextHigher: false, GetNextLower: true);
					}
					else
					{
						FirstRecord = Elements.UserAsteroids.GetAsteroidRecord_fromNumber(FirstAsteroidNumber, GetNextHigher: false, GetNextLower: true);
					}
					int.TryParse(array[1], out LastAsteroidNumber);
					if (LastAsteroidNumber == 0)
					{
						if (!UserElements)
						{
							LastRecord = Elements.MainAsteroids.AstElements.Count - 1;
						}
						else
						{
							LastRecord = Elements.UserAsteroids.AstElements.Count - 1;
						}
					}
					else if (!UserElements)
					{
						LastRecord = Elements.MainAsteroids.GetAsteroidRecord_fromNumber(LastAsteroidNumber, GetNextHigher: true, GetNextLower: false);
					}
					else
					{
						LastRecord = Elements.UserAsteroids.GetAsteroidRecord_fromNumber(LastAsteroidNumber, GetNextHigher: true, GetNextLower: false);
					}
				}
			}
			FirstAsteroidName = (LastAsteroidName = "");
			FirstAsteroidNumber = (LastAsteroidNumber = 0);
			if ((FirstRecord >= 0) & (LastRecord >= 0))
			{
				NumberOfAsteroids = LastRecord - FirstRecord + 1;
				if (!UserElements)
				{
					FirstAsteroidName = Elements.MainAsteroids.AstElements[FirstRecord].IDName.Trim();
					FirstAsteroidNumber = Elements.MainAsteroids.AstElements[FirstRecord].IDNumber;
					LastAsteroidName = Elements.MainAsteroids.AstElements[LastRecord].IDName.Trim();
					LastAsteroidNumber = Elements.MainAsteroids.AstElements[LastRecord].IDNumber;
				}
				else
				{
					FirstAsteroidName = Elements.UserAsteroids.AstElements[FirstRecord].IDName.Trim();
					FirstAsteroidNumber = Elements.UserAsteroids.AstElements[FirstRecord].IDNumber;
					LastAsteroidName = Elements.UserAsteroids.AstElements[LastRecord].IDName.Trim();
					LastAsteroidNumber = Elements.UserAsteroids.AstElements[LastRecord].IDNumber;
				}
				string text2 = "";
				if (FirstAsteroidNumber > 0)
				{
					text2 = "(" + FirstAsteroidNumber + ") ";
				}
				string text3 = "";
				if (LastAsteroidNumber > 0)
				{
					text3 = "(" + LastAsteroidNumber + ") ";
				}
				SearchLabel = text2 + FirstAsteroidName + " <=> " + text3 + LastAsteroidName;
				return true;
			}
			FirstAsteroidNumber = (LastAsteroidNumber = result);
			FirstAsteroidName = (LastAsteroidName = text);
			SearchLabel = "(" + text + ")  Horizons only";
			NumberOfAsteroids = 1;
			return true;
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
			//IL_016d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0177: Expected O, but got Unknown
			//IL_0184: Unknown result type (might be due to invalid IL or missing references)
			//IL_018e: Expected O, but got Unknown
			//IL_0dff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e09: Expected O, but got Unknown
			txtSearchString = new TextBox();
			cmdOK = new Button();
			optSelectAll = new RadioButton();
			optSelectRange = new RadioButton();
			optSelectAllNumbered = new RadioButton();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label7 = new Label();
			label8 = new Label();
			label6 = new Label();
			label9 = new Label();
			cmdCancel = new Button();
			panel1 = new Panel();
			label16 = new Label();
			label13 = new Label();
			label12 = new Label();
			label11 = new Label();
			label10 = new Label();
			label2 = new Label();
			cmdSelectFromList = new Button();
			label1 = new Label();
			panel2 = new Panel();
			label14 = new Label();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)txtSearchString).set_Location(new Point(231, 13));
			((Control)txtSearchString).set_Name("txtSearchString");
			((Control)txtSearchString).set_Size(new Size(123, 20));
			((Control)txtSearchString).set_TabIndex(0);
			((Control)txtSearchString).add_KeyDown(new KeyEventHandler(txtSearchString_KeyDown));
			((Control)txtSearchString).add_KeyPress(new KeyPressEventHandler(txtSearchString_KeyPress));
			((Control)cmdOK).set_Location(new Point(104, 404));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(49, 28));
			((Control)cmdOK).set_TabIndex(1);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)optSelectAll).set_AutoSize(true);
			((Control)optSelectAll).set_Location(new Point(10, 381));
			((Control)optSelectAll).set_Name("optSelectAll");
			((Control)optSelectAll).set_Size(new Size(158, 17));
			((Control)optSelectAll).set_TabIndex(4);
			((Control)optSelectAll).set_Text("Select all available asteroids");
			((ButtonBase)optSelectAll).set_UseVisualStyleBackColor(true);
			((Control)optSelectRange).set_AutoSize(true);
			optSelectRange.set_Checked(true);
			((Control)optSelectRange).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSelectRange).set_Location(new Point(10, 15));
			((Control)optSelectRange).set_Name("optSelectRange");
			((Control)optSelectRange).set_Size(new Size(217, 17));
			((Control)optSelectRange).set_TabIndex(5);
			optSelectRange.set_TabStop(true);
			((Control)optSelectRange).set_Text("Select one, or a range, of asteroids     =>");
			((ButtonBase)optSelectRange).set_UseVisualStyleBackColor(true);
			((Control)optSelectAllNumbered).set_AutoSize(true);
			((Control)optSelectAllNumbered).set_Location(new Point(10, 359));
			((Control)optSelectAllNumbered).set_Name("optSelectAllNumbered");
			((Control)optSelectAllNumbered).set_Size(new Size(210, 17));
			((Control)optSelectAllNumbered).set_TabIndex(6);
			((Control)optSelectAllNumbered).set_Text("Select all available Numbered asteroids");
			((ButtonBase)optSelectAllNumbered).set_UseVisualStyleBackColor(true);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(4, 3));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(267, 15));
			((Control)label3).set_TabIndex(9);
			((Control)label3).set_Text("Setting asteroid number, range, or name");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(8, 28));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(257, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("—  To select a single asteroid - set its number or name");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(8, 144));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(302, 26));
			((Control)label5).set_TabIndex(11);
			((Control)label5).set_Text("—  To select all asteroids up to a number,  preceed that number\r\nwith a dash. For example   – 125");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(8, 113));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(264, 26));
			((Control)label7).set_TabIndex(13);
			((Control)label7).set_Text("—  To select a range of asteroids, separate the first and\r\n last number with a dash. For example 25 – 100");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(8, 175));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(285, 26));
			((Control)label8).set_TabIndex(14);
			((Control)label8).set_Text("—  To select all asteroids from a number,  place a dash after\r\nthe last number. For example   2000 –");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(42, 207));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(98, 15));
			((Control)label6).set_TabIndex(15);
			((Control)label6).set_Text("Wild card  use");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(8, 82));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(277, 26));
			((Control)label9).set_TabIndex(16);
			((Control)label9).set_Text("—  For un-numbered asteroids make sure there is a space \r\nbetween the year and the identifier. eg 2021 FG145");
			((Control)cmdCancel).set_Location(new Point(210, 404));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(49, 28));
			((Control)cmdCancel).set_TabIndex(17);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)panel1).set_BackColor(Color.Beige);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label16);
			((Control)panel1).get_Controls().Add((Control)(object)label13);
			((Control)panel1).get_Controls().Add((Control)(object)label12);
			((Control)panel1).get_Controls().Add((Control)(object)label11);
			((Control)panel1).get_Controls().Add((Control)(object)label10);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)label9);
			((Control)panel1).get_Controls().Add((Control)(object)label6);
			((Control)panel1).get_Controls().Add((Control)(object)label8);
			((Control)panel1).get_Controls().Add((Control)(object)label7);
			((Control)panel1).get_Controls().Add((Control)(object)label5);
			((Control)panel1).get_Controls().Add((Control)(object)label4);
			((Control)panel1).get_Controls().Add((Control)(object)label3);
			((Control)panel1).set_Location(new Point(30, 47));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(324, 304));
			((Control)panel1).set_TabIndex(18);
			((Control)label16).set_BackColor(Color.PaleGreen);
			((Control)label16).set_Location(new Point(-1, 272));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(324, 30));
			((Control)label16).set_TabIndex(22);
			((Control)label16).set_Text("   —  To select a Comet. ONE comet only. Formats are:\r\n              nnnP          C/yyyy xxxx           P/yyyy xxxx");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(8, 64));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(299, 13));
			((Control)label13).set_TabIndex(21);
			((Control)label13).set_Text("—  To select by name, use full name, or Wild cards (see below)");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(8, 46));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(215, 13));
			((Control)label12).set_TabIndex(20);
			((Control)label12).set_Text("—  Upper/lower case for text does not matter");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(42, 254));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(188, 13));
			((Control)label11).set_TabIndex(19);
			((Control)label11).set_Text("—   *NNNN    Name ENDS with NNNN");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(42, 222));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(214, 13));
			((Control)label10).set_TabIndex(18);
			((Control)label10).set_Text("—   *NNNN*   Name CONTAINS with NNNN");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(42, 238));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(201, 13));
			((Control)label2).set_TabIndex(17);
			((Control)label2).set_Text("—    NNNN*   Name STARTS with NNNN");
			((Control)cmdSelectFromList).set_BackColor(SystemColors.ControlLight);
			((Control)cmdSelectFromList).set_ForeColor(SystemColors.ControlText);
			((Control)cmdSelectFromList).set_Location(new Point(98, 467));
			((Control)cmdSelectFromList).set_Name("cmdSelectFromList");
			((Control)cmdSelectFromList).set_Size(new Size(167, 29));
			((Control)cmdSelectFromList).set_TabIndex(19);
			((Control)cmdSelectFromList).set_Text("Select from a list of asteroids");
			((ButtonBase)cmdSelectFromList).set_UseVisualStyleBackColor(false);
			((Control)cmdSelectFromList).add_Click((EventHandler)cmdSelectFromList_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(161, 449));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(41, 13));
			((Control)label1).set_TabIndex(20);
			((Control)label1).set_Text("- OR -");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).set_Location(new Point(3, 441));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(358, 2));
			((Control)panel2).set_TabIndex(21);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label14).set_ForeColor(Color.DarkRed);
			((Control)label14).set_Location(new Point(243, 32));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(97, 13));
			((Control)label14).set_TabIndex(22);
			((Control)label14).set_Text("press Esc to cancel");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(362, 507));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdSelectFromList);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)optSelectAllNumbered);
			((Control)this).get_Controls().Add((Control)(object)optSelectRange);
			((Control)this).get_Controls().Add((Control)(object)optSelectAll);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)txtSearchString);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterIDSearch", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationAsterIDSearch);
			((Control)this).set_Name("AsteroidID_SearchString");
			((Form)this).set_StartPosition((FormStartPosition)4);
			((Control)this).set_Text("Select asteroid(s) for search");
			((Form)this).add_Load((EventHandler)AsteroidID_SearchString_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
