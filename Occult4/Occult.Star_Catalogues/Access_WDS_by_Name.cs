using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;

namespace Occult.Star_Catalogues
{
	internal class Access_WDS_by_Name
	{
		private static WDS_Indexer wIndex;

		private static List<WDS_Indexer> WDSIndex = new List<WDS_Indexer>();

		private const double Radian = 180.0 / Math.PI;

		internal static void PrepareToAccessWDS()
		{
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0076: Unknown result type (might be due to invalid IL or missing references)
			//IL_009c: Unknown result type (might be due to invalid IL or missing references)
			if (!File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				MessageBox.Show("The Washington Double Star catalogue (WDS) is not present. \r\n\r\nDownload the catalogue using the Maintenance  Downloads   page", "No WDS Catlogue");
				return;
			}
			if (File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				DateTime lastWriteTime = File.GetLastWriteTime(Utilities.AppPath + "\\DownLoaded Files\\wds.dat");
				DateTime value = DateTime.Now.AddYears(-1);
				if (lastWriteTime.CompareTo(value) < 0)
				{
					MessageBox.Show("The copy of WDS catalogue you are using is more than a year old. \r\n\r\nTo make sure you are using the latest observations, download the \r\nWDS catalogue using the   Maintenance - Downloads   page", "Old WDS Catalogue");
				}
			}
			if (!File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				MessageBox.Show("WDS catalogue is not available.", "No WDS");
				return;
			}
			WDSIndex.Clear();
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				int num = 0;
				do
				{
					wIndex = new WDS_Indexer();
					wIndex.Record = num;
					wIndex.Name = streamReader.ReadLine()!.Substring(10, 12);
					WDSIndex.Add(wIndex);
					num++;
				}
				while (!streamReader.EndOfStream);
			}
			WDS_Indexer.SortByName = true;
			WDSIndex.Sort();
		}

		internal static void EndAccessToWDS()
		{
			WDSIndex.Clear();
		}

		internal static bool ReadWDS(string Name_Comps, out string Line, out int LineInWDS, bool ReturnWDSLine)
		{
			if (!WDS_Indexer.SortByName)
			{
				WDS_Indexer.SortByName = true;
				WDSIndex.Sort();
			}
			int num = 0;
			Line = "";
			LineInWDS = 0;
			if (WDSIndex.Count < 1)
			{
				return false;
			}
			int num2 = WDSIndex.Count - 1;
			int num3 = 0;
			bool flag = false;
			if (Name_Comps.Trim().Length > 0)
			{
				do
				{
					int num4 = (num2 + num3) / 2;
					int num5 = Name_Comps.CompareTo(WDSIndex[num4].Name);
					if (num5 == 0)
					{
						flag = true;
						num = WDSIndex[num4].Record;
						LineInWDS = num4;
						break;
					}
					if (num5 > 0)
					{
						num3 = num4 + 1;
					}
					else
					{
						num2 = num4 - 1;
					}
				}
				while (num3 <= num2);
			}
			if (ReturnWDSLine && flag)
			{
				using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\wds.dat", FileMode.Open, FileAccess.Read))
				{
					BinaryReader binaryReader = new BinaryReader(fileStream);
					fileStream.Seek(132 * num, SeekOrigin.Begin);
					Line = new string(binaryReader.ReadChars(130));
					return flag;
				}
			}
			return flag;
		}

		internal static bool AddNewWDS_EntriesToXZDoubles()
		{
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_007d: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a35: Unknown result type (might be due to invalid IL or missing references)
			bool result = false;
			if (!File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				MessageBox.Show("The Washington Double Star catalogue (WDS) is not present. \r\n\r\nDownload the catalogue using the Maintenance  Downloads   page", "No WDS Catlogue");
				return result;
			}
			if (File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				DateTime lastWriteTime = File.GetLastWriteTime(Utilities.AppPath + "\\DownLoaded Files\\wds.dat");
				DateTime value = DateTime.Now.AddYears(-1);
				if (lastWriteTime.CompareTo(value) < 0)
				{
					MessageBox.Show("The copy of WDS catalogue you are using is more than a year old. \r\n\r\nTo make sure you are using the latest observations, download the \r\nWDS catalogue using the   Maintenance - Downloads   page", "Old WDS Catalogue");
				}
			}
			if (!File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				MessageBox.Show("WDS catalogue is not available.", "No WDS");
				return result;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			WDSIndex.Clear();
			string Line;
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				int num = 0;
				do
				{
					wIndex = new WDS_Indexer();
					wIndex.Record = num;
					Line = streamReader.ReadLine();
					wIndex.Name = Line.Substring(10, 12);
					if (Line.Substring(112, 6).Trim().Length > 4)
					{
						wIndex.RA = double.Parse(Line.Substring(112, 2)) + double.Parse(Line.Substring(114, 2)) / 60.0 + double.Parse(Line.Substring(116, 5).Replace(" ", "0")) / 3600.0;
						wIndex.Dec = double.Parse(Line.Substring(122, 2)) + double.Parse(Line.Substring(124, 2)) / 60.0 + double.Parse(Line.Substring(126, 4).Replace(" ", "0")) / 3600.0;
						if (Line.Substring(121, 1) == "-")
						{
							wIndex.Dec = 0.0 - wIndex.Dec;
						}
					}
					else
					{
						wIndex.RA = double.Parse(Line.Substring(0, 2)) + double.Parse(Line.Substring(2, 3)) / 600.0;
						wIndex.Dec = double.Parse(Line.Substring(6, 2)) + double.Parse(Line.Substring(8, 2)) / 60.0;
						if (Line.Substring(5, 1) == "-")
						{
							wIndex.Dec = 0.0 - wIndex.Dec;
						}
					}
					wIndex.RA /= 12.0 / Math.PI;
					wIndex.Dec /= 180.0 / Math.PI;
					WDSIndex.Add(wIndex);
					num++;
				}
				while (!streamReader.EndOfStream);
			}
			WDS_Indexer.SortByName = true;
			WDSIndex.Sort();
			for (int i = 0; i < EditXZ_Forms.DoubleList.Count; i++)
			{
				if (ReadWDS(EditXZ_Forms.DoubleList[i].IDCode.Replace("OCc", "OCC").Replace("OC*", "OCC") + EditXZ_Forms.DoubleList[i].IDNumber + EditXZ_Forms.DoubleList[i].IDPair, out Line, out var LineInWDS, ReturnWDSLine: false))
				{
					WDSIndex[LineInWDS].NameIsInXZ = true;
				}
				Application.DoEvents();
			}
			WDS_Indexer.SortByName = false;
			WDSIndex.Sort();
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\xz80.dat", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader = new BinaryReader(fileStream);
				int num2 = (int)fileStream.Length / 35;
				int num3 = 0;
				for (int j = 1; j < num2; j++)
				{
					fileStream.Seek(35 * j, SeekOrigin.Begin);
					double num4 = (double)binaryReader.ReadInt32() / 10000000.0 / (180.0 / Math.PI) + Math.PI;
					_ = (double)binaryReader.ReadSingle() / (180.0 / Math.PI);
					double num5 = (double)binaryReader.ReadInt32() / 10000000.0 / (180.0 / Math.PI);
					double num6 = num4 - 4.84813681109536E-05;
					double num7 = num4 + 4.84813681109536E-05;
					int num8 = num3;
					bool flag = false;
					do
					{
						if (WDSIndex[num8].RA < num6)
						{
							flag = true;
						}
						if (flag & (WDSIndex[num8].RA >= num6))
						{
							flag = false;
							num3 = num8;
						}
						if ((Math.Abs(WDSIndex[num8].RA - num4) < 4.84813681109536E-05) & (Math.Abs(WDSIndex[num8].Dec - num5) < 4.84813681109536E-05))
						{
							WDSIndex[num8].IsInXZ = true;
							fileStream.Seek(35 * j + 24, SeekOrigin.Begin);
							if (WDSIndex[num8].XZ == 0)
							{
								WDSIndex[num8].XZ = binaryReader.ReadInt32();
							}
							else if (WDSIndex[num8].XZ2 == 0)
							{
								WDSIndex[num8].XZ2 = binaryReader.ReadInt32();
							}
							else if (WDSIndex[num8].XZ3 == 0)
							{
								WDSIndex[num8].XZ3 = binaryReader.ReadInt32();
							}
							else if (WDSIndex[num8].XZ4 == 0)
							{
								WDSIndex[num8].XZ4 = binaryReader.ReadInt32();
							}
						}
						if (WDSIndex[num8].RA > num7)
						{
							break;
						}
						num8++;
					}
					while (num8 < WDSIndex.Count - 1);
				}
			}
			int num9 = 0;
			using (FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\DownLoaded Files\\wds.dat", FileMode.Open, FileAccess.Read))
			{
				BinaryReader binaryReader2 = new BinaryReader(fileStream2);
				using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Downloaded Files\\WDS Stars not in XZDoubles.txt");
				streamWriter.WriteLine("This file lists pairs in WDS that were not matched to pairs in XZDoubles on " + DateTime.Now.ToShortDateString() + "\r\n");
				for (int k = 0; k < WDSIndex.Count; k++)
				{
					if (!(WDSIndex[k].IsInXZ & !WDSIndex[k].NameIsInXZ))
					{
						continue;
					}
					fileStream2.Seek(132 * WDSIndex[k].Record, SeekOrigin.Begin);
					Line = new string(binaryReader2.ReadChars(130));
					streamWriter.WriteLine(WDSIndex[k].XZ.ToString().PadLeft(6) + " " + WDSIndex[k].XZ2.ToString().PadLeft(6) + " " + WDSIndex[k].XZ3.ToString().PadLeft(6) + " " + WDSIndex[k].XZ4.ToString().PadLeft(6) + " " + Line);
					if (num9 < 50)
					{
						XZDoubles xZDoubles = new XZDoubles();
						xZDoubles.Decode_WDS2006_Line(Line);
						if (WDSIndex[k].XZ2 == 0)
						{
							int num11 = (xZDoubles.XZPrimary = (xZDoubles.XZPrinciple = WDSIndex[k].XZ));
						}
						else if (WDSIndex[k].XZ3 == 0)
						{
							if (WDSIndex[k].XZ < WDSIndex[k].XZ2)
							{
								int num11 = (xZDoubles.XZPrimary = (xZDoubles.XZPrinciple = WDSIndex[k].XZ));
								xZDoubles.XZSecondary = WDSIndex[k].XZ2;
							}
							else if (WDSIndex[k].XZ3 == 0)
							{
								int num11 = (xZDoubles.XZPrimary = (xZDoubles.XZPrinciple = WDSIndex[k].XZ2));
								xZDoubles.XZSecondary = WDSIndex[k].XZ;
							}
						}
						else if ((WDSIndex[k].XZ < WDSIndex[k].XZ2) & (WDSIndex[k].XZ2 < WDSIndex[k].XZ3))
						{
							int num11 = (xZDoubles.XZPrimary = (xZDoubles.XZPrinciple = WDSIndex[k].XZ));
							xZDoubles.XZSecondary = WDSIndex[k].XZ2;
						}
						else if ((WDSIndex[k].XZ > WDSIndex[k].XZ2) & (WDSIndex[k].XZ2 > WDSIndex[k].XZ3))
						{
							int num11 = (xZDoubles.XZPrimary = (xZDoubles.XZPrinciple = WDSIndex[k].XZ3));
							xZDoubles.XZSecondary = WDSIndex[k].XZ2;
						}
						else
						{
							int num11 = (xZDoubles.XZPrimary = (xZDoubles.XZPrinciple = WDSIndex[k].XZ));
							xZDoubles.XZSecondary = WDSIndex[k].XZ2;
						}
						EditXZ_Forms.DoubleList.Add(xZDoubles);
						result = true;
					}
					num9++;
					if (num9 == 50)
					{
						MessageBox.Show("50 events have been added to the current list.\r\nNo more events will be added.\r\n\r\nCheck currently added items for duplicates and XZ references,\r\nand then save.\r\n\r\nReload the file, and repeat until no more lines are added.\r\n\r\nA full list of stars to be added is located in the Downloads directory\r\nin the file 'WDS Stars not in XZDoubles.txt'", "Added star limit reached.");
					}
				}
			}
			EditXZ_Forms.DoubleList.Sort();
			EditXZ_Forms.DoubleEditor.PopulateDoublesList(0);
			Cursor.set_Current(Cursors.get_Default());
			return result;
		}
	}
}
