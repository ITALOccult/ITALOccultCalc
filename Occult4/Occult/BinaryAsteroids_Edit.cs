using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class BinaryAsteroids_Edit : Form
	{
		private readonly string AppPath;

		private readonly string BinaryElementFile;

		public int FirstRecord;

		public int LastRecord;

		private const int ElementFileLength = 143;

		private float[] Dias;

		private int[] AstNum;

		private string[] AstName;

		private int NumAsteroids;

		private float H0;

		private bool UnsavedEdits;

		private IContainer components;

		private GroupBox grpEdit;

		private Button cmdAddElements;

		private Button cmdReplaceSelected;

		private Label label18;

		private ComboBox cmbRefFrame;

		private Label label17;

		private Label label16;

		private Label label15;

		private Label label14;

		private Label label11;

		private Label label10;

		private Label label9;

		private Label label8;

		private Label label7;

		private Label label4;

		private TextBox txtMaxD;

		private TextBox txtPerihelion;

		private TextBox txtNode;

		private TextBox txtI;

		private TextBox txte;

		private TextBox txtA;

		private NumericUpDown updnDay;

		private NumericUpDown updnMonth;

		private NumericUpDown updnYear;

		private TextBox txtMA;

		private TextBox txtName;

		private TextBox txtNumber;

		private ListBox lstElements;

		private Label label1;

		private Label label2;

		private Label label3;

		private TextBox txtIdentifier;

		private Label label5;

		private TextBox txtPeriod;

		private Button cmdDelete;

		private Label label6;

		private Button cmdH0toDia;

		private TextBox txt_diafromH0;

		private Button cmdFindAstNumber;

		private Button cmdGetFromWeb;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem openToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdGetSatIDs;

		public BinaryAsteroids_Edit()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			BinaryElementFile = AppPath + "\\Resource Files\\BinaryAsteroids.csv";
		}

		private void BinaryAsteroids_Edit_Load(object sender, EventArgs e)
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
			DisplayBinaryElements();
			if (BinaryAsteroids.BinElements.Count > 0)
			{
				((ListControl)lstElements).set_SelectedIndex(0);
			}
		}

		private void DisplayBinaryElements()
		{
			lstElements.get_Items().Clear();
			if (BinaryAsteroids.BinElements.Count < 1)
			{
				BinaryAsteroids.Fill_AllAsteroids();
			}
			for (int i = 0; i < BinaryAsteroids.BinElements.Count; i++)
			{
				lstElements.get_Items().Add((object)BinaryAsteroids.BinElements[i].ToString());
			}
		}

		private void lstElements_SelectedIndexChanged(object sender, EventArgs e)
		{
			BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
			int selectedIndex = ((ListControl)lstElements).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				binaryAsteroidElements.ReadElementLine(lstElements.get_Items().get_Item(selectedIndex).ToString());
				((Control)txtNumber).set_Text(binaryAsteroidElements.IDAsteroidNumber.ToString());
				((Control)txtIdentifier).set_Text(binaryAsteroidElements.IDAsteroidIdentifier.ToString());
				((Control)txtName).set_Text(binaryAsteroidElements.IDMoonName.Trim());
				updnYear.set_Value((decimal)binaryAsteroidElements.Year);
				updnMonth.set_Value((decimal)binaryAsteroidElements.Month);
				updnDay.set_Value((decimal)binaryAsteroidElements.Day);
				((Control)txtPeriod).set_Text(binaryAsteroidElements.PeriodDays.ToString());
				((Control)txtMA).set_Text(binaryAsteroidElements.Meananomaly.ToString());
				((Control)txtPerihelion).set_Text(binaryAsteroidElements.Perihelion.ToString());
				((Control)txtNode).set_Text(binaryAsteroidElements.Node.ToString());
				((Control)txtI).set_Text(binaryAsteroidElements.I.ToString());
				((Control)txte).set_Text(binaryAsteroidElements.E.ToString());
				((Control)txtA).set_Text(binaryAsteroidElements.A.ToString());
				((Control)txtMaxD).set_Text(binaryAsteroidElements.Diameter.ToString());
				((ListControl)cmbRefFrame).set_SelectedIndex(binaryAsteroidElements.ReferenceFrame);
				((Control)txt_diafromH0).set_Text("");
			}
		}

		private void cmdReplaceSelected_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to REPLACE", "Check Replace", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				AddEditBinaryEntry(Replace: true);
			}
		}

		private void cmdAddElements_Click(object sender, EventArgs e)
		{
			AddEditBinaryEntry(Replace: false);
		}

		private void AddEditBinaryEntry(bool Replace)
		{
			BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
			double.TryParse(((Control)txtNumber).get_Text(), out var result);
			string iDAsteroidIdentifier = ((Control)txtIdentifier).get_Text().Trim().PadRight(16)
				.Substring(0, 16);
			string iDMoonName = ((Control)txtName).get_Text().Trim().PadRight(21)
				.Substring(0, 21);
			int year = (int)updnYear.get_Value();
			int month = (int)updnMonth.get_Value();
			double day = (double)updnDay.get_Value();
			if (!double.TryParse(((Control)txtPeriod).get_Text(), out var result2))
			{
				result2 = 1.0;
			}
			if (!double.TryParse(((Control)txtMA).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			if (!double.TryParse(((Control)txtPerihelion).get_Text(), out var result4))
			{
				result4 = 0.0;
			}
			if (!double.TryParse(((Control)txtNode).get_Text(), out var result5))
			{
				result5 = 0.0;
			}
			if (!double.TryParse(((Control)txtI).get_Text(), out var result6))
			{
				result6 = 0.0;
			}
			if (!double.TryParse(((Control)txte).get_Text(), out var result7))
			{
				result7 = 0.0;
			}
			if (!double.TryParse(((Control)txtA).get_Text(), out var result8))
			{
				result8 = 0.0;
			}
			if (!double.TryParse(((Control)txtMaxD).get_Text(), out var result9))
			{
				result9 = 0.0;
			}
			int num = ((ListControl)cmbRefFrame).get_SelectedIndex();
			if (num < 0)
			{
				num = 0;
			}
			binaryAsteroidElements.IDAsteroidNumber = result;
			binaryAsteroidElements.IDAsteroidIdentifier = iDAsteroidIdentifier;
			binaryAsteroidElements.IDMoonName = iDMoonName;
			binaryAsteroidElements.Year = year;
			binaryAsteroidElements.Month = month;
			binaryAsteroidElements.Day = day;
			binaryAsteroidElements.PeriodDays = result2;
			binaryAsteroidElements.Meananomaly = result3;
			binaryAsteroidElements.Perihelion = result4;
			binaryAsteroidElements.Node = result5;
			binaryAsteroidElements.I = result6;
			binaryAsteroidElements.E = result7;
			binaryAsteroidElements.A = result8;
			binaryAsteroidElements.Diameter = result9;
			binaryAsteroidElements.ReferenceFrame = num;
			if (!Replace)
			{
				BinaryAsteroids.BinElements.Add(binaryAsteroidElements);
				BinaryAsteroids.BinElements.Sort();
				DisplayBinaryElements();
			}
			else
			{
				BinaryAsteroids.BinElements.RemoveAt(((ListControl)lstElements).get_SelectedIndex());
				BinaryAsteroids.BinElements.Add(binaryAsteroidElements);
				BinaryAsteroids.BinElements.Sort();
				DisplayBinaryElements();
			}
			UnsavedEdits = true;
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			if (lstElements.get_Items().get_Count() < 1)
			{
				return;
			}
			string text = AppPath + "\\Resource Files\\BinaryAsteroidElements_Previous.dat";
			if (File.Exists(text))
			{
				File.Delete(text);
			}
			File.Move(BinaryElementFile, text);
			StreamWriter streamWriter = new StreamWriter(BinaryElementFile);
			lstElements.set_Sorted(true);
			int num = 0;
			for (int i = 0; i < lstElements.get_Items().get_Count(); i++)
			{
				if (lstElements.get_Items().get_Item(i).ToString()!.Substring(0, 7).Trim() != "")
				{
					num = i;
					break;
				}
			}
			lstElements.set_Sorted(false);
			for (int i = 0; i < num - 1; i++)
			{
				string text2 = lstElements.get_Items().get_Item(i).ToString()!.Substring(8, 10);
				string text3 = lstElements.get_Items().get_Item(i + 1).ToString()!.Substring(8, 10);
				if (!(text2.Substring(0, 6) == text3.Substring(0, 6)))
				{
					continue;
				}
				if (!int.TryParse(text2.Substring(7), out var result))
				{
					result = 0;
				}
				if (!int.TryParse(text3.Substring(7), out var result2))
				{
					result2 = 0;
				}
				if (result2 < result)
				{
					string text4 = lstElements.get_Items().get_Item(i).ToString();
					lstElements.get_Items().RemoveAt(i);
					lstElements.get_Items().Insert(i + 1, (object)text4);
					if (i > 0)
					{
						i--;
					}
				}
			}
			for (int i = num; i < lstElements.get_Items().get_Count(); i++)
			{
				streamWriter.WriteLine(lstElements.get_Items().get_Item(i));
			}
			for (int i = 0; i < num; i++)
			{
				if (lstElements.get_Items().get_Item(i).ToString()!.Substring(0, 7).Trim() == "")
				{
					streamWriter.WriteLine(lstElements.get_Items().get_Item(i));
				}
			}
			lstElements.set_Sorted(true);
			streamWriter.Close();
		}

		private void BinaryAsteroids_Edit_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_004f: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to Delete \r\n\r\n" + lstElements.get_Items().get_Item(((ListControl)lstElements).get_SelectedIndex()).ToString()!.Substring(0, 46).Trim(), "Check Delete", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				BinaryAsteroids.BinElements.RemoveAt(((ListControl)lstElements).get_SelectedIndex());
				DisplayBinaryElements();
				UnsavedEdits = true;
			}
		}

		private void cmdH0toDia_Click(object sender, EventArgs e)
		{
			int result = 0;
			if (!int.TryParse(((Control)txtNumber).get_Text(), out result))
			{
				result = 0;
			}
			string text = ((Control)txtIdentifier).get_Text().Trim();
			if (result > 0)
			{
				if (NumAsteroids < 1)
				{
					Read_MPCORB_Astorb();
				}
				for (int i = 0; i < NumAsteroids; i++)
				{
					if (result == AstNum[i])
					{
						((Control)txt_diafromH0).set_Text(string.Format("{0,1:f1}", Dias[i]));
						break;
					}
				}
				return;
			}
			for (int j = 0; j < NumAsteroids; j++)
			{
				if (text == AstName[j])
				{
					((Control)txt_diafromH0).set_Text(string.Format("{0,1:f1}", Dias[j]));
					break;
				}
			}
		}

		private void cmdGetFromWeb_Click(object sender, EventArgs e)
		{
			//IL_004a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0050: Invalid comparison between Unknown and I4
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac4: Unknown result type (might be due to invalid IL or missing references)
			ArrayList arrayList = new ArrayList();
			ArrayList arrayList2 = new ArrayList();
			ArrayList arrayList3 = new ArrayList();
			new List<IAUMoonIdentifier>();
			string text = "";
			string text2 = "";
			bool flag = false;
			int num = 0;
			int num2 = 0;
			string[] array = new string[6];
			if ((int)MessageBox.Show("This routine retrieves data for asteroidal satellites from the\r\nAsteroids with Satellites Database - Johnston Archive\r\n\r\nThe process requires internet access, and will take several minutes to query the several hundred individual web pages.\r\n\r\nYour existing file 'binaryelements.csv' will be renamed to 'binaryelements.old'\r\n\r\nClick OK to procede.", "Confirm", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 2)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("Generating a new list requires internet access. You are not connected to the internet.\r\n\r\nYou must be connect to the internet before the list can be generated.", "No internet");
				((Form)this).Close();
			}
			GetSatelliteIDs();
			string text3;
			try
			{
				text3 = http.Download_WebPage("http://www.johnstonsarchive.net/astro/asteroidmoons.html");
			}
			catch
			{
				MessageBox.Show("Download of index page failed. \r\nThe generation of the list cannot continue.");
				return;
			}
			if (File.Exists(AppPath + "\\Resource Files\\BinaryAsteroids.old"))
			{
				File.Delete(AppPath + "\\Resource Files\\BinaryAsteroids.old");
			}
			if (File.Exists(AppPath + "\\Resource Files\\BinaryAsteroids.csv"))
			{
				File.Move(AppPath + "\\Resource Files\\BinaryAsteroids.csv", AppPath + "\\Resource Files\\BinaryAsteroids.old");
			}
			int num3 = text3.IndexOf("<b>by designation:</b>");
			int num4 = text3.IndexOf("<a href=amsources.html>");
			int num5 = num3 + 10;
			int num6 = num5;
			do
			{
				num5 = text3.IndexOf("href=", num5) + 5;
				if (num5 > num4)
				{
					break;
				}
				num6 = text3.IndexOf(">", num5);
				arrayList.Add(text3.Substring(num5, num6 - num5));
			}
			while (num5 < num4);
			lstElements.get_Items().Clear();
			using (StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\BinaryAsteroids.csv"))
			{
				BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
				streamWriter.WriteLine(binaryAsteroidElements.BinaryHeader);
				for (int i = 1; i < arrayList.Count; i++)
				{
					((Control)this).set_Text("Edit Binary asteroid details" + string.Format("         Retrieving asteroid {0,1} of {1,1}", i, arrayList.Count));
					string text4;
					try
					{
						text4 = http.Download_WebPage("http://www.johnstonsarchive.net/astro/" + arrayList[i]!.ToString());
					}
					catch
					{
						text2 = text2 + arrayList[i]!.ToString() + "\r\n";
						num2++;
						continue;
					}
					arrayList2.Clear();
					arrayList3.Clear();
					for (int j = 0; j < 6; j++)
					{
						array[j] = "";
					}
					int num7 = 0;
					int startIndex = 0;
					int num8 = 0;
					num7 = text4.IndexOf("<title>", startIndex);
					int num9 = text4.IndexOf("<meta name=", num7);
					if (num9 > num7)
					{
						num7 = text4.IndexOf("<title>", num9);
					}
					startIndex = text4.IndexOf("</title>", num7);
					text4.Contains("(9069)");
					text4.Contains("(136108)");
					text4.Contains("(136617)");
					text4.Contains("(153591)");
					text4.Contains("1998 WW31");
					string[] array2 = text4.Substring(num7, startIndex - num7).Trim().Replace('\r', ' ')
						.Replace('\n', ' ')
						.Replace("e>", "e>\n ")
						.Replace("</", "\n</")
						.Replace("and ", ",")
						.Replace("  ", "")
						.Replace(", ,", ",")
						.Replace(",,", ",")
						.Split(new char[1] { '\n' });
					string[] array3 = array2[1].Split(new char[1] { ',' });
					flag = false;
					if ((array2.Length > 1) & (array3[0].Trim().Length == 0))
					{
						array3 = array2[2].Split(new char[1] { ',' });
						flag = true;
					}
					array3[0] = array3[0].Trim();
					num5 = array3[0].IndexOf(")");
					num = ((num5 <= 0) ? ((int)GetNumber(array3[0])) : ((int)GetNumber(array3[0].Substring(0, num5))));
					if (num == 10199)
					{
						continue;
					}
					if (num5 < array3[0].Length - 1)
					{
						text = array3[0].Substring(num5 + 1).Trim();
						if (text.Contains("[hst5]"))
						{
							continue;
						}
						for (int k = 1; k < array3.Length; k++)
						{
							array[k] = array3[k].Trim();
						}
					}
					else if (array2.Length > 1 && !flag)
					{
						string[] array4 = array2[2].Split(new char[1] { ',' });
						text = array4[0].Trim();
						if (array4.Length > 1)
						{
							for (int l = 1; l < array4.Length; l++)
							{
								array[l] = array4[l].Trim();
							}
							if (array2.Length > 2 && array2[2].Length > 5 && array2[2].Substring(0, 4) == "and ")
							{
								if (array[array4.Length - 1].Trim().Length == 0)
								{
									array[array4.Length - 1] = array2[3].Substring(4);
								}
								else
								{
									array[array4.Length] = array2[3].Substring(4);
								}
							}
						}
						else
						{
							for (int m = 1; m < array2.Length - 2; m++)
							{
								array[m] = array2[m + 2].Trim();
							}
						}
					}
					for (int n = 1; n < 6; n++)
					{
						if (array[n].Length >= 5)
						{
							if (array[n].Substring(0, 4) == "and ")
							{
								array[n] = array[n].Substring(4);
							}
							num5 = array[n].IndexOf("second");
							if (num5 >= 0)
							{
								array[n] = array[n].Remove(num5, 4).Insert(num5, "2");
							}
							num5 = array[n].IndexOf("third");
							if (num5 >= 0)
							{
								array[n] = array[n].Remove(num5, 3).Insert(num5, "3");
							}
							num5 = array[n].IndexOf("(\"");
							if (num5 >= 0)
							{
								array[n] = array[n].Substring(0, num5).Trim();
							}
							array[n] = array[n].Replace("\"", "").Trim();
						}
					}
					text4.Substring(num7, startIndex - num7).Split(new string[2] { "\n", "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
					do
					{
						num7 = text4.IndexOf("orbital data", startIndex);
						if (num7 < 0)
						{
							break;
						}
						num8++;
						startIndex = text4.IndexOf("</table>", num7);
						string[] value = text4.Substring(num7, startIndex - num7).Split(new string[2] { "\n", "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
						arrayList2.Add(value);
					}
					while (num7 > 0);
					do
					{
						num7 = text4.IndexOf("other data", startIndex);
						if (num7 < 0)
						{
							break;
						}
						startIndex = text4.IndexOf("</table>", num7);
						string[] value2 = text4.Substring(num7, startIndex - num7).Split(new string[2] { "\n", "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
						arrayList3.Add(value2);
					}
					while (num7 > 0);
					for (int num10 = 1; num10 < num8; num10++)
					{
						binaryAsteroidElements = new BinaryAsteroidElements();
						binaryAsteroidElements.IDAsteroidNumber = num;
						binaryAsteroidElements.IDAsteroidIdentifier = text;
						binaryAsteroidElements.IDMoonName = array[num10].Trim();
						binaryAsteroidElements.Year = 1999;
						binaryAsteroidElements.Month = 1;
						binaryAsteroidElements.Day = 1.0;
						binaryAsteroidElements.ReferenceFrame = 2;
						string[] array5 = (string[])arrayList2[num10];
						for (int num11 = 0; num11 < array5.Length; num11++)
						{
							if (array5[num11].Contains("semimajor"))
							{
								binaryAsteroidElements.A = GetNumber(array5[num11 + 1]);
							}
							if (array5[num11].Contains("orbital period "))
							{
								binaryAsteroidElements.PeriodDays = GetNumber(array5[num11 + 1]);
							}
							if (array5[num11].Contains("eccentricity"))
							{
								binaryAsteroidElements.E = GetNumber(array5[num11 + 1]);
							}
							if (array5[num11].Contains("inclination "))
							{
								binaryAsteroidElements.I = GetNumber(array5[num11]);
							}
							if (array5[num11].Contains("ascending node"))
							{
								binaryAsteroidElements.Node = GetNumber(array5[num11]);
							}
							if (array5[num11].Contains("of pericenter"))
							{
								binaryAsteroidElements.Perihelion = GetNumber(array5[num11]);
							}
							if (array5[num11].Contains("mean anomaly"))
							{
								binaryAsteroidElements.Meananomaly = GetNumber(array5[num11]);
							}
							if (array5[num11].Contains("Epoch") | array5[num11].Contains("pericenter passage"))
							{
								num5 = array5[num11].IndexOf("<td>") + 4;
								num6 = array5[num11].IndexOf("</td>", num5);
								string text5 = array5[num11].Substring(num5, num6 - num5);
								num5 = text5.IndexOf("&");
								if (num5 > 0)
								{
									text5 = text5.Substring(0, num5);
								}
								binaryAsteroidElements.Year = (int)GetNumber(text5.Substring(0, 4));
								binaryAsteroidElements.Month = "JanFebMarAprMayJunJulAugSepOctNovDec".IndexOf(text5.Substring(5, 3)) / 3 + 1;
								if (binaryAsteroidElements.Month < 1)
								{
									binaryAsteroidElements.Month = 1;
								}
								binaryAsteroidElements.Day = GetNumber(text5.Substring(9));
							}
						}
						if (binaryAsteroidElements.Meananomaly < 0.0)
						{
							binaryAsteroidElements.Meananomaly += 360.0;
						}
						string[] array6 = (string[])arrayList3[num10 + 1];
						for (int num12 = 0; num12 < array6.Length; num12++)
						{
							if (array6[num12].Contains("diameter") & !array6[num12].Contains("ratio"))
							{
								binaryAsteroidElements.Diameter = GetNumber(array6[num12 + 1]);
								break;
							}
						}
						((ListControl)lstElements).set_SelectedIndex(lstElements.get_Items().Add((object)binaryAsteroidElements.ToString()));
						streamWriter.WriteLine(binaryAsteroidElements.ToCSVString());
						Application.DoEvents();
					}
				}
			}
			((Control)this).set_Text("Edit Binary asteroid details");
			if (num2 > 0)
			{
				MessageBox.Show("The routine failed to download the web pages for " + num2 + " asteroids.\r\nThe missing asteroids are:\r\n" + text2 + "This may be caused by invalid links on Johnston's web page.", "Missed downloads", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			else
			{
				MessageBox.Show("The list has been successfully generated", "Success", (MessageBoxButtons)0, (MessageBoxIcon)64);
				BinaryAsteroids.BinElements.Clear();
				BinaryAsteroids.Fill_AllAsteroids();
				DisplayBinaryElements();
			}
		}

		internal void GetSatelliteIDs()
		{
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			//IL_021e: Unknown result type (might be due to invalid IL or missing references)
			string text;
			try
			{
				text = http.Download_WebPage("http://www.johnstonsarchive.net/astro/binastnames.html");
			}
			catch
			{
				MessageBox.Show("Download of the IAU identifications summary page failed. \r\nThe generation of the list cannot continue.");
				return;
			}
			int startIndex = text.IndexOf("<tr><th>number");
			startIndex = text.IndexOf("<tr><td>", startIndex);
			int num = text.IndexOf("<tr><th>number", startIndex);
			string[] array = text.Substring(startIndex, num - startIndex).Replace("\r", "").Split(new char[1] { '\n' });
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\BinaryAsteroid_SatelliteNames.txt");
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Length < 50)
				{
					continue;
				}
				string[] array2 = array[i].Replace(" nowrap", "").Replace("<tr><td>", "").Replace("</tr></td>", "")
					.Replace("&nbsp;", "")
					.Replace("</td><td>", "~")
					.Split(new char[1] { '~' });
				if (array2.Length < 7)
				{
					continue;
				}
				string text2 = array2[0].Trim();
				if (text2.Length <= 1)
				{
					continue;
				}
				_ = text2 == "229762";
				string text3 = array2[5].Replace("V", "5").Replace("IV", "4").Replace("III", "3")
					.Replace("II", "2")
					.Replace("I", "1")
					.Trim();
				string text4 = array2[6].Trim();
				string text5 = array2[7].Trim();
				if ((text3.Length > 0) | (text5.Length > 0))
				{
					IAUMoonIdentifier iAUMoonIdentifier = new IAUMoonIdentifier();
					iAUMoonIdentifier.AstNumber = int.Parse(text2);
					iAUMoonIdentifier.MoonID = text3;
					if (text4.Length > 1)
					{
						iAUMoonIdentifier.MoonName = text4;
					}
					else
					{
						iAUMoonIdentifier.MoonName = text5;
					}
					streamWriter.WriteLine(iAUMoonIdentifier);
				}
			}
			MessageBox.Show("Satellite ID's have been written to the file\r\n\r\n" + Utilities.AppPath + "\\Resource Files\\BinaryAsteroid_SatelliteNames.txt", "Names downloaded", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdGetSatIDs_Click(object sender, EventArgs e)
		{
			GetSatelliteIDs();
		}

		private double GetNumber(string Num)
		{
			Num = Num.Trim();
			if (Num.Length < 1)
			{
				return 0.0;
			}
			int num = Num.IndexOf("<td>");
			if (num >= 0)
			{
				Num = Num.Substring(num + 4);
			}
			int num2 = Num.IndexOf("&");
			if (num2 >= 0)
			{
				Num = Num.Substring(0, num2);
			}
			num2 = Num.IndexOf("d");
			if (num2 >= 0)
			{
				Num = Num.Substring(0, num2);
			}
			if (Num.Length < 1)
			{
				return 0.0;
			}
			if (Num.Substring(0, 1) == "(")
			{
				Num = Num.Substring(1);
			}
			int length = Num.Length;
			if (Num.Substring(length - 1) == ")")
			{
				Num = Num.Substring(0, length - 1);
			}
			if (Num.Substring(0, 1) == "<")
			{
				Num = Num.Substring(1);
			}
			if (Num.Substring(0, 1) == ">")
			{
				Num = Num.Substring(1);
			}
			if (!double.TryParse(Num, out var result))
			{
				return 0.0;
			}
			return result;
		}

		private void openToolStripMenuItem_Click(object sender, EventArgs e)
		{
			BinaryAsteroids.BinElements.Clear();
			BinaryAsteroids.Fill_AllAsteroids();
			DisplayBinaryElements();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save();
		}

		private void Save()
		{
			using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\BinaryAsteroids.csv");
			BinaryAsteroidElements binaryAsteroidElements = new BinaryAsteroidElements();
			streamWriter.WriteLine(binaryAsteroidElements.BinaryHeader);
			for (int i = 0; i < BinaryAsteroids.BinElements.Count; i++)
			{
				streamWriter.WriteLine(BinaryAsteroids.BinElements[i].ToCSVString());
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Binary asteroids - edit");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void BinaryAsteroids_Edit_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0022: Invalid comparison between Unknown and I4
			//IL_002b: Invalid comparison between Unknown and I4
			if (UnsavedEdits)
			{
				DialogResult val = MessageBox.Show("You have made changes to the data that have not been saved.\r\n\r\nDo you want to save the changes?", "Unsaved edits", (MessageBoxButtons)3, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				if ((int)val == 6)
				{
					Save();
				}
				if ((int)val == 2)
				{
					((CancelEventArgs)(object)e).Cancel = true;
				}
			}
		}

		private void Read_MPCORB_Astorb()
		{
			//IL_004a: Unknown result type (might be due to invalid IL or missing references)
			string path = Utilities.AppPath + "\\Downloaded files\\MPCORB.dat";
			int num = 203;
			bool flag = true;
			if (!File.Exists(path))
			{
				path = Utilities.AppPath + "\\Downloaded files\\Astorb.dat";
				num = 268;
				flag = false;
				if (!File.Exists(path))
				{
					MessageBox.Show("This functionality requires a current version of either MPCORB or \r\nAstorb. These can be downloaded from the Downloads page.", "You need MPCORB or Astorb");
					return;
				}
			}
			int num2 = 0;
			float num3 = 0f;
			using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Resource files\\Asteroid Diameters.dat");
			string? text = streamReader.ReadLine();
			num2 = int.Parse(text!.Substring(0, 7));
			num3 = float.Parse(text!.Substring(9, 6));
			bool flag2 = false;
			using StreamReader streamReader2 = new StreamReader(path);
			NumAsteroids = (int)streamReader2.BaseStream.Length / num;
			H0 = 0f;
			Dias = new float[NumAsteroids];
			AstNum = new int[NumAsteroids];
			AstName = new string[NumAsteroids];
			NumAsteroids = 0;
			if (flag)
			{
				string text2;
				do
				{
					text2 = streamReader2.ReadLine()!.PadRight(5);
				}
				while (text2.Substring(0, 5) != "-----");
			}
			do
			{
				string text2 = streamReader2.ReadLine();
				if (text2.Length == 0)
				{
					continue;
				}
				Dias[NumAsteroids] = 0f;
				if (flag)
				{
					if (!int.TryParse(text2.Substring(166, 7).Replace("(", ""), out AstNum[NumAsteroids]))
					{
						AstNum[NumAsteroids] = 0;
					}
					AstName[NumAsteroids] = text2.PadRight(193).Substring(175, 18).Trim();
					if (!float.TryParse(text2.Substring(8, 5), out H0))
					{
						H0 = 0f;
					}
				}
				else
				{
					if (!int.TryParse(text2.Substring(0, 6), out AstNum[NumAsteroids]))
					{
						AstNum[NumAsteroids] = 0;
					}
					AstName[NumAsteroids] = text2.Substring(7, 18).Trim();
					if (!float.TryParse(text2.Substring(42, 5), out H0))
					{
						H0 = 0f;
					}
				}
				if (H0 > 0f)
				{
					Dias[NumAsteroids] = (float)Math.Pow(10.0, 3.52 - 0.2 * (double)H0);
				}
				if (AstNum[NumAsteroids] > 0)
				{
					if (!flag2)
					{
						while (num2 < AstNum[NumAsteroids])
						{
							flag2 = streamReader.EndOfStream;
							if (flag2)
							{
								num2 = 0;
								num3 = 0f;
								break;
							}
							string? text3 = streamReader.ReadLine();
							num2 = int.Parse(text3!.Substring(0, 7));
							num3 = float.Parse(text3!.Substring(9, 6));
						}
					}
					if ((num3 > 0f) & (num2 == AstNum[NumAsteroids]))
					{
						Dias[NumAsteroids] = num3;
					}
				}
				NumAsteroids++;
			}
			while (!streamReader2.EndOfStream);
		}

		private void cmdFindAstNumber_Click(object sender, EventArgs e)
		{
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			string text = ((Control)txtIdentifier).get_Text().Trim();
			bool flag = false;
			if (NumAsteroids < 1)
			{
				Read_MPCORB_Astorb();
			}
			if (NumAsteroids < 1)
			{
				return;
			}
			for (int i = 0; i < NumAsteroids; i++)
			{
				if (text == AstName[i])
				{
					if (AstNum[i] > 0)
					{
						((Control)txtNumber).set_Text(AstNum[i].ToString());
					}
					flag = true;
					break;
				}
			}
			if (!flag)
			{
				MessageBox.Show("Asteroid " + text + " has not been found. This means either\r\n* the asteroid has not been numbered, or\r\n* the asteroid has been numbered AND 'named'.\r\n\r\nIf you think the asteroid has been numbered, use the MPC Ephemeris service to identify this asteroid.");
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
			//IL_1948: Unknown result type (might be due to invalid IL or missing references)
			//IL_1952: Expected O, but got Unknown
			//IL_19b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_19bc: Expected O, but got Unknown
			//IL_19c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_19ce: Expected O, but got Unknown
			grpEdit = new GroupBox();
			cmdFindAstNumber = new Button();
			label6 = new Label();
			cmdH0toDia = new Button();
			txt_diafromH0 = new TextBox();
			cmdDelete = new Button();
			label5 = new Label();
			txtPeriod = new TextBox();
			label3 = new Label();
			txtIdentifier = new TextBox();
			cmdAddElements = new Button();
			cmdReplaceSelected = new Button();
			label18 = new Label();
			cmbRefFrame = new ComboBox();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			label14 = new Label();
			label11 = new Label();
			label10 = new Label();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			label4 = new Label();
			txtMaxD = new TextBox();
			txtPerihelion = new TextBox();
			txtNode = new TextBox();
			txtI = new TextBox();
			txte = new TextBox();
			txtA = new TextBox();
			updnDay = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnYear = new NumericUpDown();
			txtMA = new TextBox();
			txtName = new TextBox();
			txtNumber = new TextBox();
			cmdGetFromWeb = new Button();
			lstElements = new ListBox();
			label1 = new Label();
			label2 = new Label();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			openToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdGetSatIDs = new Button();
			((Control)grpEdit).SuspendLayout();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdFindAstNumber);
			((Control)grpEdit).get_Controls().Add((Control)(object)label6);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdH0toDia);
			((Control)grpEdit).get_Controls().Add((Control)(object)txt_diafromH0);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdDelete);
			((Control)grpEdit).get_Controls().Add((Control)(object)label5);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtPeriod);
			((Control)grpEdit).get_Controls().Add((Control)(object)label3);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtIdentifier);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdAddElements);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdReplaceSelected);
			((Control)grpEdit).get_Controls().Add((Control)(object)label18);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbRefFrame);
			((Control)grpEdit).get_Controls().Add((Control)(object)label17);
			((Control)grpEdit).get_Controls().Add((Control)(object)label16);
			((Control)grpEdit).get_Controls().Add((Control)(object)label15);
			((Control)grpEdit).get_Controls().Add((Control)(object)label14);
			((Control)grpEdit).get_Controls().Add((Control)(object)label11);
			((Control)grpEdit).get_Controls().Add((Control)(object)label10);
			((Control)grpEdit).get_Controls().Add((Control)(object)label9);
			((Control)grpEdit).get_Controls().Add((Control)(object)label8);
			((Control)grpEdit).get_Controls().Add((Control)(object)label7);
			((Control)grpEdit).get_Controls().Add((Control)(object)label4);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtMaxD);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtPerihelion);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtNode);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtI);
			((Control)grpEdit).get_Controls().Add((Control)(object)txte);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtA);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnDay);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnMonth);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnYear);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtMA);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtName);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtNumber);
			((Control)grpEdit).set_Location(new Point(3, 532));
			((Control)grpEdit).set_Name("grpEdit");
			((Control)grpEdit).set_Size(new Size(1065, 144));
			((Control)grpEdit).set_TabIndex(0);
			grpEdit.set_TabStop(false);
			((Control)grpEdit).set_Text("Edit the elements");
			((Control)cmdFindAstNumber).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFindAstNumber).set_Location(new Point(98, 118));
			((Control)cmdFindAstNumber).set_Name("cmdFindAstNumber");
			((Control)cmdFindAstNumber).set_Size(new Size(137, 21));
			((Control)cmdFindAstNumber).set_TabIndex(36);
			((Control)cmdFindAstNumber).set_Text("Identify Asteroid number");
			((ButtonBase)cmdFindAstNumber).set_UseVisualStyleBackColor(true);
			((Control)cmdFindAstNumber).add_Click((EventHandler)cmdFindAstNumber_Click);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(500, 105));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(20, 13));
			((Control)label6).set_TabIndex(35);
			((Control)label6).set_Text("km");
			((Control)cmdH0toDia).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdH0toDia).set_Location(new Point(351, 118));
			((Control)cmdH0toDia).set_Name("cmdH0toDia");
			((Control)cmdH0toDia).set_Size(new Size(137, 21));
			((Control)cmdH0toDia).set_TabIndex(34);
			((Control)cmdH0toDia).set_Text("Retrieve asteroid diameter");
			((ButtonBase)cmdH0toDia).set_UseVisualStyleBackColor(true);
			((Control)cmdH0toDia).add_Click((EventHandler)cmdH0toDia_Click);
			((Control)txt_diafromH0).set_Location(new Point(493, 118));
			((Control)txt_diafromH0).set_Name("txt_diafromH0");
			((Control)txt_diafromH0).set_Size(new Size(40, 20));
			((Control)txt_diafromH0).set_TabIndex(33);
			((Control)cmdDelete).set_Location(new Point(967, 37));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(100, 35));
			((Control)cmdDelete).set_TabIndex(29);
			((Control)cmdDelete).set_Text("Delete selected elements");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(245, 94));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(68, 13));
			((Control)label5).set_TabIndex(10);
			((Control)label5).set_Text("Period (days)");
			((Control)txtPeriod).set_Location(new Point(315, 91));
			((Control)txtPeriod).set_Name("txtPeriod");
			((Control)txtPeriod).set_Size(new Size(84, 20));
			((Control)txtPeriod).set_TabIndex(10);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(9, 66));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(87, 13));
			((Control)label3).set_TabIndex(2);
			((Control)label3).set_Text("Asteroid identifier");
			((Control)txtIdentifier).set_Location(new Point(98, 63));
			((TextBoxBase)txtIdentifier).set_MaxLength(22);
			((Control)txtIdentifier).set_Name("txtIdentifier");
			((Control)txtIdentifier).set_Size(new Size(128, 20));
			((Control)txtIdentifier).set_TabIndex(3);
			((Control)cmdAddElements).set_Location(new Point(864, 35));
			((Control)cmdAddElements).set_Name("cmdAddElements");
			((Control)cmdAddElements).set_Size(new Size(100, 35));
			((Control)cmdAddElements).set_TabIndex(27);
			((Control)cmdAddElements).set_Text("Add as a new object");
			((ButtonBase)cmdAddElements).set_UseVisualStyleBackColor(true);
			((Control)cmdAddElements).add_Click((EventHandler)cmdAddElements_Click);
			((Control)cmdReplaceSelected).set_Location(new Point(864, 76));
			((Control)cmdReplaceSelected).set_Name("cmdReplaceSelected");
			((Control)cmdReplaceSelected).set_Size(new Size(100, 35));
			((Control)cmdReplaceSelected).set_TabIndex(28);
			((Control)cmdReplaceSelected).set_Text("Replace selected elements");
			((ButtonBase)cmdReplaceSelected).set_UseVisualStyleBackColor(true);
			((Control)cmdReplaceSelected).add_Click((EventHandler)cmdReplaceSelected_Click);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(694, 93));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(56, 13));
			((Control)label18).set_TabIndex(25);
			((Control)label18).set_Text("Ref. frame");
			cmbRefFrame.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbRefFrame).set_FormattingEnabled(true);
			cmbRefFrame.get_Items().AddRange(new object[5] { "Unspecified", "Ecliptic J2000", "Equator J2000", "Ecliptic B1950", "Equator B1950" });
			((Control)cmbRefFrame).set_Location(new Point(752, 90));
			((Control)cmbRefFrame).set_Name("cmbRefFrame");
			((Control)cmbRefFrame).set_Size(new Size(98, 21));
			((Control)cmbRefFrame).set_TabIndex(26);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(266, 38));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(47, 13));
			((Control)label17).set_TabIndex(6);
			((Control)label17).set_Text("diameter");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(529, 66));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(52, 13));
			((Control)label16).set_TabIndex(19);
			((Control)label16).set_Text("perihelion");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(550, 94));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(31, 13));
			((Control)label15).set_TabIndex(21);
			((Control)label15).set_Text("node");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(414, 66));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(9, 13));
			((Control)label14).set_TabIndex(13);
			((Control)label14).set_Text("i");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(32, 94));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(65, 13));
			((Control)label11).set_TabIndex(4);
			((Control)label11).set_Text("Moon Name");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(580, 38));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(38, 13));
			((Control)label10).set_TabIndex(15);
			((Control)label10).set_Text("Epoch");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(687, 66));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(77, 13));
			((Control)label9).set_TabIndex(23);
			((Control)label9).set_Text("Mean Anomaly");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(411, 38));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(13, 13));
			((Control)label8).set_TabIndex(11);
			((Control)label8).set_Text("e");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(278, 66));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(36, 13));
			((Control)label7).set_TabIndex(8);
			((Control)label7).set_Text("a (km)");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(14, 38));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(83, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Asteroid number");
			((Control)txtMaxD).set_Location(new Point(315, 35));
			((Control)txtMaxD).set_Name("txtMaxD");
			((Control)txtMaxD).set_Size(new Size(41, 20));
			((Control)txtMaxD).set_TabIndex(7);
			((Control)txtPerihelion).set_Location(new Point(583, 63));
			((Control)txtPerihelion).set_Name("txtPerihelion");
			((Control)txtPerihelion).set_Size(new Size(84, 20));
			((Control)txtPerihelion).set_TabIndex(20);
			((Control)txtNode).set_Location(new Point(583, 91));
			((Control)txtNode).set_Name("txtNode");
			((Control)txtNode).set_Size(new Size(84, 20));
			((Control)txtNode).set_TabIndex(22);
			((Control)txtI).set_Location(new Point(426, 63));
			((Control)txtI).set_Name("txtI");
			((Control)txtI).set_Size(new Size(84, 20));
			((Control)txtI).set_TabIndex(14);
			((Control)txte).set_Location(new Point(426, 35));
			((Control)txte).set_Name("txte");
			((Control)txte).set_Size(new Size(84, 20));
			((Control)txte).set_TabIndex(12);
			((Control)txtA).set_Location(new Point(315, 63));
			((Control)txtA).set_Name("txtA");
			((Control)txtA).set_Size(new Size(84, 20));
			((Control)txtA).set_TabIndex(9);
			updnDay.set_DecimalPlaces(5);
			((Control)updnDay).set_Location(new Point(714, 35));
			updnDay.set_Maximum(new decimal(new int[4] { 3199999, 0, 0, 327680 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(66, 20));
			((Control)updnDay).set_TabIndex(18);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Location(new Point(674, 35));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(34, 20));
			((Control)updnMonth).set_TabIndex(17);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnYear).set_Location(new Point(621, 35));
			updnYear.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(47, 20));
			((Control)updnYear).set_TabIndex(16);
			updnYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			((Control)txtMA).set_Location(new Point(766, 63));
			((Control)txtMA).set_Name("txtMA");
			((Control)txtMA).set_Size(new Size(84, 20));
			((Control)txtMA).set_TabIndex(24);
			((Control)txtName).set_Location(new Point(99, 91));
			((TextBoxBase)txtName).set_MaxLength(21);
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(128, 20));
			((Control)txtName).set_TabIndex(5);
			((Control)txtNumber).set_Location(new Point(99, 35));
			((TextBoxBase)txtNumber).set_MaxLength(7);
			((Control)txtNumber).set_Name("txtNumber");
			((Control)txtNumber).set_Size(new Size(63, 20));
			((Control)txtNumber).set_TabIndex(1);
			((Control)cmdGetFromWeb).set_Location(new Point(14, 29));
			((Control)cmdGetFromWeb).set_Name("cmdGetFromWeb");
			((Control)cmdGetFromWeb).set_Size(new Size(146, 39));
			((Control)cmdGetFromWeb).set_TabIndex(37);
			((Control)cmdGetFromWeb).set_Text("Generate new list using \r\n'Johnston's Archive'");
			((ButtonBase)cmdGetFromWeb).set_UseVisualStyleBackColor(true);
			((Control)cmdGetFromWeb).add_Click((EventHandler)cmdGetFromWeb_Click);
			((Control)lstElements).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstElements).set_FormattingEnabled(true);
			lstElements.set_ItemHeight(14);
			((Control)lstElements).set_Location(new Point(3, 87));
			((Control)lstElements).set_Name("lstElements");
			((Control)lstElements).set_Size(new Size(1076, 438));
			((Control)lstElements).set_TabIndex(2);
			lstElements.add_SelectedIndexChanged((EventHandler)lstElements_SelectedIndexChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(10, 60));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(1043, 14));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("                                               diam         a     Period      e          inclin        epoch       M.A.     perihelion  node     Ref");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(5, 71));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(1057, 14));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text(" number Identifier       Satellite Name         (km)       (km)     days                   o           y m  d        o          o          o     Frame");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1080, 24));
			((Control)menuStrip1).set_TabIndex(38);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)openToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(61, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...     ");
			((ToolStripItem)openToolStripMenuItem).set_Image((Image)Resources.OpenSelectedItem);
			((ToolStripItem)openToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)openToolStripMenuItem).set_Name("openToolStripMenuItem");
			((ToolStripItem)openToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)openToolStripMenuItem).set_Text("Re-load from file");
			((ToolStripItem)openToolStripMenuItem).add_Click((EventHandler)openToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(180, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdGetSatIDs).set_Location(new Point(186, 29));
			((Control)cmdGetSatIDs).set_Name("cmdGetSatIDs");
			((Control)cmdGetSatIDs).set_Size(new Size(118, 38));
			((Control)cmdGetSatIDs).set_TabIndex(39);
			((Control)cmdGetSatIDs).set_Text("Get satellite ID,s\r\nJohnston's Archive");
			((ButtonBase)cmdGetSatIDs).set_UseVisualStyleBackColor(true);
			((Control)cmdGetSatIDs).add_Click((EventHandler)cmdGetSatIDs_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1080, 678));
			((Control)this).get_Controls().Add((Control)(object)cmdGetSatIDs);
			((Control)this).get_Controls().Add((Control)(object)cmdGetFromWeb);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)lstElements);
			((Control)this).get_Controls().Add((Control)(object)grpEdit);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterBinary", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationAsterBinary);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("BinaryAsteroids_Edit");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("Edit Binary asteroid details");
			((Form)this).add_FormClosing(new FormClosingEventHandler(BinaryAsteroids_Edit_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(BinaryAsteroids_Edit_FormClosed));
			((Form)this).add_Load((EventHandler)BinaryAsteroids_Edit_Load);
			((Control)grpEdit).ResumeLayout(false);
			((Control)grpEdit).PerformLayout();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
